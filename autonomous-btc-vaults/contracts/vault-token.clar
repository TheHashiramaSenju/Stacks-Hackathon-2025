;; AutonomousBTC Vaults - Advanced Governance Token with DeFi Features
;; Enterprise-grade token with staking, delegation, yield farming, voting-escrow, and vesting

(define-trait sip-010-trait
  (
    ;; SIP-010 trait definition
    (get-name () (response (string-utf8 32) uint))
    (get-symbol () (response (string-utf8 32) uint))
    (get-decimals () (response uint uint))
    (get-total-supply () (response uint uint))
    (get-balance (principal) (response uint uint))
    (transfer (uint principal principal (optional (buff 34))) (response bool uint))
    (transfer-from (uint principal principal (optional (buff 34))) (response bool uint))
    (approve (principal uint) (response bool uint))
    (get-allowance (principal principal) (response uint uint))
  )
)


;; =========================
;; Constants
;; =========================

(define-constant TOKEN-NAME "AutonomousBTC Governance Token")
(define-constant TOKEN-SYMBOL "ABTC-GOV")
(define-constant TOKEN-DECIMALS u6)
(define-constant TOKEN-MAX-SUPPLY u1000000000000) ;; 1,000,000.000000 (base-10^6)
(define-constant CONTRACT-OWNER tx-sender) ;; deployer captured at deploy-time

;; Staking and Yield
(define-constant BASE-APY u800)                   ;; 8% base APY (bp)
(define-constant MAX-STAKE-MULTIPLIER u300)       ;; 3x
(define-constant CONVICTION-PERIODS (list u144 u432 u1008 u2016 u4032)) ;; 1,3,7,14,28 days
(define-constant CONVICTION-MULTIPLIERS (list u110 u125 u150 u200 u300)) ;; 1.1x,1.25x,1.5x,2x,3x

;; Error constants (response errors)
(define-constant ERR-UNAUTHORIZED (err u401))
(define-constant ERR-NOT-TOKEN-OWNER (err u402))
(define-constant ERR-INSUFFICIENT-BALANCE (err u403))
(define-constant ERR-INVALID-AMOUNT (err u404))
(define-constant ERR-STAKE-LOCKED (err u405))
(define-constant ERR-INVALID-LOCK-PERIOD (err u406))
(define-constant ERR-DELEGATION-EXISTS (err u407))
(define-constant ERR-COOLDOWN-ACTIVE (err u408))
(define-constant ERR-INSUFFICIENT-REWARDS (err u409))
(define-constant ERR-INVALID-MULTIPLIER (err u410))
(define-constant ERR-POOL-INACTIVE (err u411))
(define-constant ERR-NO-LIQUIDITY (err u412))
(define-constant ERR-OVER-ALLOWANCE (err u413))
(define-constant ERR-VESTING-CLIFF (err u414))
(define-constant ERR-VESTING-NOT-FOUND (err u415))
(define-constant ERR-VESTING-REVOKED (err u416))
(define-constant ERR-NO-POSITION (err u417))

;; =========================
;; Data variables
;; =========================

(define-data-var total-staked uint u0)
(define-data-var total-rewards-distributed uint u0)
(define-data-var staking-enabled bool true)
(define-data-var delegation-enabled bool true)
(define-data-var unstake-cooldown-period uint u432) ;; 3 days
(define-data-var min-stake-amount uint u100000) ;; 0.1 tokens (6 decimals)
(define-data-var pool-counter uint u0)
(define-data-var next-vesting-schedule-id uint u0)

;; =========================
;; SIP-010 FT
;; =========================

(define-fungible-token abtc-gov)

;; =========================
;; Maps
;; =========================

;; Allowance (SIP-010 allowance extension)
(define-map allowances { owner: principal, spender: principal } uint)

;; Staking
(define-map staker-info 
  { staker: principal }
  {
    staked-amount: uint,
    stake-timestamp: uint,
    lock-period: uint,
    conviction-multiplier: uint,
    rewards-earned: uint,
    last-reward-claim: uint,
    unstake-request-time: (optional uint),
    total-rewards-claimed: uint
  }
)

;; Delegation
(define-map delegation-info
  { delegator: principal }
  {
    delegatee: principal,
    amount: uint,
    timestamp: uint,
    active: bool
  }
)

(define-map delegatee-stats
  { delegatee: principal }
  {
    total-delegated: uint,
    delegator-count: uint,
    commission-rate: uint, ;; bp
    reputation-score: uint,
    total-commission-earned: uint
  }
)

;; Liquidity Pools in governance token
(define-map liquidity-pools
  { pool-id: uint }
  {
    token-pair: (string-ascii 32),
    total-liquidity: uint,
    apy: uint,
    active: bool,
    created-at: uint,
    rewards-per-block: uint
  }
)

(define-map user-liquidity-positions
  { user: principal, pool-id: uint }
  {
    liquidity-amount: uint,
    rewards-earned: uint,
    entry-timestamp: uint,
    last-claim: uint
  }
)

;; Voting Escrow
(define-map voting-escrow
  { user: principal }
  {
    locked-amount: uint,
    unlock-time: uint,
    voting-power: uint,
    lock-duration: uint
  }
)

;; Governance Rewards (per proposal)
(define-map governance-rewards
  { user: principal, proposal-id: uint }
  {
    reward-amount: uint,
    claimed: bool,
    participation-score: uint
  }
)

;; Vesting schedules
(define-map vesting-schedules
  { beneficiary: principal, schedule-id: uint }
  {
    total-amount: uint,
    released-amount: uint,
    start-time: uint,
    cliff-duration: uint,
    vesting-duration: uint,
    revocable: bool,
    revoked: bool
  }
)

;; Optional team allocations
(define-map team-allocations
  { team-member: principal }
  {
    allocation: uint,
    vested: uint,
    cliff-passed: bool,
    vesting-start: uint
  }
)

;; =========================
;; SIP-010 Read-only
;; =========================

(define-read-only (get-name) (ok TOKEN-NAME))
(define-read-only (get-symbol) (ok TOKEN-SYMBOL))
(define-read-only (get-decimals) (ok TOKEN-DECIMALS))
(define-read-only (get-balance (who principal)) (ok (ft-get-balance abtc-gov who)))
(define-read-only (get-total-supply) (ok (ft-get-supply abtc-gov)))
(define-read-only (get-token-uri) (ok (some u"https://autonomousbtc.vaults/metadata.json")))

;; =========================
;; SIP-010 Allowance Extension
;; =========================

(define-public (approve (spender principal) (amount uint))
  (begin
    (map-set allowances { owner: tx-sender, spender: spender } amount)
    (print { event: "approve", owner: tx-sender, spender: spender, amount: amount })
    (ok true)
  )
)

(define-read-only (get-allowance (owner principal) (spender principal))
  (ok (default-to u0 (map-get? allowances { owner: owner, spender: spender })))
)

(define-public (transfer-from (amount uint) (owner principal) (recipient principal) (memo (optional (buff 34))))
  (let (
    (allowed (default-to u0 (map-get? allowances { owner: owner, spender: tx-sender })))
  )
    (asserts! (>= allowed amount) ERR-OVER-ALLOWANCE)
    (map-set allowances { owner: owner, spender: tx-sender } (- allowed amount))
    (try! (ft-transfer? abtc-gov amount owner recipient))
    (print { event: "transfer-from", owner: owner, spender: tx-sender, recipient: recipient, amount: amount, memo: memo })
    (ok true)
  )
)

;; =========================
;; SIP-010 Transfer
;; =========================

(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
  (begin
    (asserts! (or (is-eq tx-sender sender) (is-eq contract-caller sender)) ERR-UNAUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (let ((sender-balance (ft-get-balance abtc-gov sender)))
      (asserts! (>= sender-balance amount) ERR-INSUFFICIENT-BALANCE)
      (try! (ft-transfer? abtc-gov amount sender recipient))
      (print { event: "transfer", sender: sender, recipient: recipient, amount: amount, memo: memo })
      (ok true)
    )
  )
)

;; =========================
;; Internal helpers
;; =========================

(define-private (get-conviction-multiplier (lock-period uint))
  (if (>= lock-period u4032) u300
    (if (>= lock-period u2016) u200
      (if (>= lock-period u1008) u150
        (if (>= lock-period u432) u125
          (if (>= lock-period u144) u110 u100)
        )
      )
    )
  )
)

;; This returns (response bool ...) to be used with try!
;; This returns (response bool uint) to be used with try! - changed to never return an error 
(define-private (update-staking-rewards (user principal))
  (match (map-get? staker-info { staker: user })
    current-stake 
    (let (
      (staked-amount (get staked-amount current-stake))
      (last-claim (get last-reward-claim current-stake))
      (time-diff (- block-height last-claim))
      (multiplier (get conviction-multiplier current-stake))
      (reward-rate (/ (* BASE-APY multiplier) u10000))
      (block-reward (/ (* staked-amount reward-rate) u52560))
      (total-reward (* block-reward time-diff))
    )
      (if (> staked-amount u0)
        (begin
          (map-set staker-info
            { staker: user }
            (merge current-stake {
              rewards-earned: (+ (get rewards-earned current-stake) total-reward),
              last-reward-claim: block-height
            })
          )
          (ok true)
        )
        (ok true)
      )
    )
    (ok true) ;; no stake: nothing to do
  )
)

(define-public (stake-tokens (amount uint) (lock-period uint))
  (let (
    (user-balance (ft-get-balance abtc-gov tx-sender))
    (multiplier (get-conviction-multiplier lock-period))
    (current-stake (default-to 
      { staked-amount: u0, stake-timestamp: u0, lock-period: u0, conviction-multiplier: u100,
        rewards-earned: u0, last-reward-claim: block-height, unstake-request-time: none, total-rewards-claimed: u0 }
      (map-get? staker-info { staker: tx-sender })
    ))
  )
    (asserts! (var-get staking-enabled) ERR-UNAUTHORIZED)
    (asserts! (>= user-balance amount) ERR-INSUFFICIENT-BALANCE)
    (asserts! (>= amount (var-get min-stake-amount)) ERR-INVALID-AMOUNT)
    (asserts! (> multiplier u0) ERR-INVALID-LOCK-PERIOD)

    (unwrap! (update-staking-rewards tx-sender) ERR-INVALID-AMOUNT)
    (try! (ft-transfer? abtc-gov amount tx-sender (as-contract tx-sender)))
    (map-set staker-info
      { staker: tx-sender }
      {
        staked-amount: (+ (get staked-amount current-stake) amount),
        stake-timestamp: block-height,
        lock-period: lock-period,
        conviction-multiplier: multiplier,
        rewards-earned: (get rewards-earned current-stake),
        last-reward-claim: block-height,
        unstake-request-time: none,
        total-rewards-claimed: (get total-rewards-claimed current-stake)
      }
    )
    (var-set total-staked (+ (var-get total-staked) amount))
    (print { event: "stake", user: tx-sender, amount: amount, lock-period: lock-period, multiplier: multiplier })
    (ok amount)
  )
)

(define-public (request-unstake (amount uint))
  (let (
    (current-stake (unwrap! (map-get? staker-info { staker: tx-sender }) ERR-INSUFFICIENT-BALANCE))
    (staked-amount (get staked-amount current-stake))
    (lock-end (+ (get stake-timestamp current-stake) (get lock-period current-stake)))
  )
    (asserts! (>= staked-amount amount) ERR-INSUFFICIENT-BALANCE)
    (asserts! (>= block-height lock-end) ERR-STAKE-LOCKED)
    (unwrap! (update-staking-rewards tx-sender) ERR-INVALID-AMOUNT)
    (map-set staker-info
      { staker: tx-sender }
      (merge current-stake { unstake-request-time: (some block-height) })
    )
    (print { event: "unstake-request", user: tx-sender, amount: amount })
    (ok true)
  )
)

(define-public (complete-unstake (amount uint))
  (let (
    (current-stake (unwrap! (map-get? staker-info { staker: tx-sender }) ERR-INSUFFICIENT-BALANCE))
    (unstake-request-time (unwrap! (get unstake-request-time current-stake) ERR-UNAUTHORIZED))
    (cooldown-end (+ unstake-request-time (var-get unstake-cooldown-period)))
    (staked-amount (get staked-amount current-stake))
  )
    (asserts! (>= block-height cooldown-end) ERR-COOLDOWN-ACTIVE)
    (asserts! (>= staked-amount amount) ERR-INSUFFICIENT-BALANCE)
    (unwrap! (update-staking-rewards tx-sender) ERR-INVALID-AMOUNT)
    (try! (as-contract (ft-transfer? abtc-gov amount (as-contract tx-sender) tx-sender)))
    (map-set staker-info
      { staker: tx-sender }
      (merge current-stake {
        staked-amount: (- staked-amount amount),
        unstake-request-time: (if (is-eq (- staked-amount amount) u0) none (get unstake-request-time current-stake)),
        last-reward-claim: block-height
      })
    )
    (var-set total-staked (- (var-get total-staked) amount))
    (print { event: "unstake-complete", user: tx-sender, amount: amount })
    (ok amount)
  )
)

(define-public (claim-staking-rewards)
  (let (
    (current-stake (unwrap! (map-get? staker-info { staker: tx-sender }) ERR-INSUFFICIENT-BALANCE))
  )
    (unwrap! (update-staking-rewards tx-sender) ERR-INVALID-AMOUNT)
    (let (
      (updated-stake (unwrap! (map-get? staker-info { staker: tx-sender }) ERR-INSUFFICIENT-BALANCE))
      (rewards-to-claim (get rewards-earned updated-stake))
      (new-supply (+ (ft-get-supply abtc-gov) rewards-to-claim))
    )
      (asserts! (> rewards-to-claim u0) ERR-INSUFFICIENT-REWARDS)
      (asserts! (<= new-supply TOKEN-MAX-SUPPLY) ERR-INVALID-AMOUNT)
      (try! (ft-mint? abtc-gov rewards-to-claim tx-sender))
      (map-set staker-info
        { staker: tx-sender }
        (merge updated-stake {
          rewards-earned: u0,
          last-reward-claim: block-height,
          total-rewards-claimed: (+ (get total-rewards-claimed updated-stake) rewards-to-claim)
        })
      )
      (var-set total-rewards-distributed (+ (var-get total-rewards-distributed) rewards-to-claim))
      (print { event: "claim-rewards", user: tx-sender, amount: rewards-to-claim })
      (ok rewards-to-claim)
    )
  )
)

;; =========================
;; Delegation
;; =========================

(define-public (delegate-voting-power (delegatee principal) (amount uint))
  (let (
    (delegator-balance (ft-get-balance abtc-gov tx-sender))
    (current-delegation (map-get? delegation-info { delegator: tx-sender }))
    (delegatee-stats-current (default-to
      { total-delegated: u0, delegator-count: u0, commission-rate: u500, reputation-score: u100, total-commission-earned: u0 }
      (map-get? delegatee-stats { delegatee: delegatee })
    ))
  )
    (asserts! (var-get delegation-enabled) ERR-UNAUTHORIZED)
    (asserts! (>= delegator-balance amount) ERR-INSUFFICIENT-BALANCE)
    (asserts! (not (is-eq tx-sender delegatee)) ERR-INVALID-AMOUNT)
    (asserts! (is-none current-delegation) ERR-DELEGATION-EXISTS)
    (map-set delegation-info
      { delegator: tx-sender }
      { delegatee: delegatee, amount: amount, timestamp: block-height, active: true }
    )
    (map-set delegatee-stats
      { delegatee: delegatee }
      {
        total-delegated: (+ (get total-delegated delegatee-stats-current) amount),
        delegator-count: (+ (get delegator-count delegatee-stats-current) u1),
        commission-rate: (get commission-rate delegatee-stats-current),
        reputation-score: (get reputation-score delegatee-stats-current),
        total-commission-earned: (get total-commission-earned delegatee-stats-current)
      }
    )
    (print { event: "delegate", delegator: tx-sender, delegatee: delegatee, amount: amount })
    (ok true)
  )
)

(define-public (undelegate-voting-power)
  (let (
    (delegation (unwrap! (map-get? delegation-info { delegator: tx-sender }) ERR-UNAUTHORIZED))
    (delegatee (get delegatee delegation))
    (amount (get amount delegation))
    (stats (unwrap! (map-get? delegatee-stats { delegatee: delegatee }) ERR-UNAUTHORIZED))
  )
    (map-delete delegation-info { delegator: tx-sender })
    (map-set delegatee-stats
      { delegatee: delegatee }
      {
        total-delegated: (- (get total-delegated stats) amount),
        delegator-count: (- (get delegator-count stats) u1),
        commission-rate: (get commission-rate stats),
        reputation-score: (get reputation-score stats),
        total-commission-earned: (get total-commission-earned stats)
      }
    )
    (print { event: "undelegate", delegator: tx-sender, delegatee: delegatee, amount: amount })
    (ok true)
  )
)

;; =========================
;; Liquidity Pools (token-based)
;; =========================

(define-public (create-liquidity-pool (token-pair (string-ascii 32)) (initial-apy uint))
  (let ((new-pool-id (+ (var-get pool-counter) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (> initial-apy u0) ERR-INVALID-AMOUNT)
    (var-set pool-counter new-pool-id)
    (map-set liquidity-pools
      { pool-id: new-pool-id }
      {
        token-pair: token-pair,
        total-liquidity: u0,
        apy: initial-apy,
        active: true,
        created-at: block-height,
        rewards-per-block: (/ initial-apy u52560)
      }
    )
    (print { event: "pool-created", pool-id: new-pool-id, token-pair: token-pair, apy: initial-apy })
    (ok new-pool-id)
  )
)

(define-private (update-liquidity-rewards (user principal) (pool-id uint))
  (match (map-get? liquidity-pools { pool-id: pool-id })
    pool
    (match (map-get? user-liquidity-positions { user: user, pool-id: pool-id })
      pos
      (let (
        (last-claim (get last-claim pos))
        (time-diff (- block-height last-claim))
        (user-liq (get liquidity-amount pos))
        (total-liq (get total-liquidity pool))
        (rpb (get rewards-per-block pool))
      )
        (if (and (> total-liq u0) (> user-liq u0))
          (let (
            (user-share (/ (* user-liq u1000000) total-liq))
            (new-reward (/ (* (* rpb time-diff) user-share) u1000000))
          )
            (map-set user-liquidity-positions
              { user: user, pool-id: pool-id }
              (merge pos {
                rewards-earned: (+ (get rewards-earned pos) new-reward),
                last-claim: block-height
              })
            )
            (ok true)
          )
          (begin
            (map-set user-liquidity-positions
              { user: user, pool-id: pool-id }
              (merge pos { last-claim: block-height })
            )
            (ok true)
          )
        )
      )
      (ok true)
    )
    (ok true)
  )
)

(define-public (provide-liquidity (pool-id uint) (amount uint))
  (let (
    (pool-info (unwrap! (map-get? liquidity-pools { pool-id: pool-id }) ERR-INVALID-AMOUNT))
    (user-balance (ft-get-balance abtc-gov tx-sender))
    (current-position (default-to
      { liquidity-amount: u0, rewards-earned: u0, entry-timestamp: block-height, last-claim: block-height }
      (map-get? user-liquidity-positions { user: tx-sender, pool-id: pool-id })
    ))
  )
    (asserts! (get active pool-info) ERR-POOL-INACTIVE)
    (asserts! (>= user-balance amount) ERR-INSUFFICIENT-BALANCE)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (unwrap! (update-liquidity-rewards tx-sender pool-id) ERR-INVALID-AMOUNT)
    (try! (ft-transfer? abtc-gov amount tx-sender (as-contract tx-sender)))
    (map-set user-liquidity-positions
      { user: tx-sender, pool-id: pool-id }
      {
        liquidity-amount: (+ (get liquidity-amount current-position) amount),
        rewards-earned: (get rewards-earned current-position),
        entry-timestamp: (get entry-timestamp current-position),
        last-claim: block-height
      }
    )
    (map-set liquidity-pools
      { pool-id: pool-id }
      (merge pool-info { total-liquidity: (+ (get total-liquidity pool-info) amount) })
    )
    (print { event: "liquidity-provided", user: tx-sender, pool-id: pool-id, amount: amount })
    (ok true)
  )
)

(define-public (withdraw-liquidity (pool-id uint) (amount uint))
  (let (
    (pool-info (unwrap! (map-get? liquidity-pools { pool-id: pool-id }) ERR-INVALID-AMOUNT))
    (pos (unwrap! (map-get? user-liquidity-positions { user: tx-sender, pool-id: pool-id }) ERR-NO-POSITION))
    (user-liq (get liquidity-amount pos))
  )
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (>= user-liq amount) ERR-INVALID-AMOUNT)
    (unwrap! (update-liquidity-rewards tx-sender pool-id) ERR-INVALID-AMOUNT)
    (try! (as-contract (ft-transfer? abtc-gov amount (as-contract tx-sender) tx-sender)))
    (map-set user-liquidity-positions
      { user: tx-sender, pool-id: pool-id }
      (merge pos {
        liquidity-amount: (- user-liq amount),
        last-claim: block-height
      })
    )
    (map-set liquidity-pools
      { pool-id: pool-id }
      (merge pool-info { total-liquidity: (- (get total-liquidity pool-info) amount) })
    )
    (print { event: "liquidity-withdrawn", user: tx-sender, pool-id: pool-id, amount: amount })
    (ok true)
  )
)

(define-public (claim-liquidity-rewards (pool-id uint))
  (let (
    (pool-info (unwrap! (map-get? liquidity-pools { pool-id: pool-id }) ERR-INVALID-AMOUNT))
    (pos (unwrap! (map-get? user-liquidity-positions { user: tx-sender, pool-id: pool-id }) ERR-NO-POSITION))
  )
    (unwrap! (update-liquidity-rewards tx-sender pool-id) ERR-INVALID-AMOUNT)
    (let (
      (updated (unwrap! (map-get? user-liquidity-positions { user: tx-sender, pool-id: pool-id }) ERR-NO-POSITION))
      (earned (get rewards-earned updated))
      (new-supply (+ (ft-get-supply abtc-gov) earned))
    )
      (asserts! (> earned u0) ERR-INSUFFICIENT-REWARDS)
      (asserts! (<= new-supply TOKEN-MAX-SUPPLY) ERR-INVALID-AMOUNT)
      (try! (ft-mint? abtc-gov earned tx-sender))
      (map-set user-liquidity-positions
        { user: tx-sender, pool-id: pool-id }
        (merge updated { rewards-earned: u0, last-claim: block-height })
      )
      (print { event: "lp-claim", user: tx-sender, pool-id: pool-id, amount: earned })
      (ok earned)
    )
  )
)

;; =========================
;; Voting Escrow
;; =========================

(define-public (lock-for-vote (amount uint) (lock-duration uint))
  (let ((bal (ft-get-balance abtc-gov tx-sender)))
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (>= bal amount) ERR-INSUFFICIENT-BALANCE)
    (let (
      (unlock (+ block-height lock-duration))
      (vp (/ (* amount lock-duration) u100))
    )
      (try! (ft-transfer? abtc-gov amount tx-sender (as-contract tx-sender)))
      (map-set voting-escrow
        { user: tx-sender }
        { locked-amount: amount, unlock-time: unlock, voting-power: vp, lock-duration: lock-duration }
      )
      (print { event: "ve-lock", user: tx-sender, amount: amount, unlock: unlock, vp: vp })
      (ok true)
    )
  )
)

(define-public (unlock-vote)
  (let ((ve (unwrap! (map-get? voting-escrow { user: tx-sender }) ERR-UNAUTHORIZED)))
    (asserts! (>= block-height (get unlock-time ve)) ERR-STAKE-LOCKED)
    (let ((amt (get locked-amount ve)))
      (asserts! (> amt u0) ERR-INVALID-AMOUNT)
      (try! (as-contract (ft-transfer? abtc-gov amt (as-contract tx-sender) tx-sender)))
      (map-delete voting-escrow { user: tx-sender })
      (print { event: "ve-unlock", user: tx-sender, amount: amt })
      (ok amt)
    )
  )
)

;; =========================
;; Governance Rewards
;; =========================

(define-public (award-governance-reward (user principal) (proposal-id uint) (amount uint) (participation uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (map-set governance-rewards
      { user: user, proposal-id: proposal-id }
      { reward-amount: amount, claimed: false, participation-score: participation }
    )
    (print { event: "gov-reward-awarded", user: user, proposal: proposal-id, amount: amount })
    (ok true)
  )
)

(define-public (claim-governance-reward (proposal-id uint))
  (match (map-get? governance-rewards { user: tx-sender, proposal-id: proposal-id })
    gr
    (let ((amt (get reward-amount gr)))
      (asserts! (not (get claimed gr)) ERR-INVALID-AMOUNT)
      (asserts! (> amt u0) ERR-INSUFFICIENT-REWARDS)
      (asserts! (<= (+ (ft-get-supply abtc-gov) amt) TOKEN-MAX-SUPPLY) ERR-INVALID-AMOUNT)
      (try! (ft-mint? abtc-gov amt tx-sender))
      (map-set governance-rewards
        { user: tx-sender, proposal-id: proposal-id }
        (merge gr { claimed: true })
      )
      (print { event: "gov-reward-claimed", user: tx-sender, proposal: proposal-id, amount: amt })
      (ok amt)
    )
    ERR-INVALID-AMOUNT
  )
)

;; =========================
;; Vesting
;; =========================

(define-public (create-vesting-schedule
  (beneficiary principal)
  (total-amount uint)
  (start-time uint)
  (cliff-duration uint)
  (vesting-duration uint)
  (revocable bool))
  (let ((new-id (+ (var-get next-vesting-schedule-id) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (> total-amount u0) ERR-INVALID-AMOUNT)
    (try! (ft-transfer? abtc-gov total-amount tx-sender (as-contract tx-sender)))
    (map-set vesting-schedules
      { beneficiary: beneficiary, schedule-id: new-id }
      {
        total-amount: total-amount,
        released-amount: u0,
        start-time: start-time,
        cliff-duration: cliff-duration,
        vesting-duration: vesting-duration,
        revocable: revocable,
        revoked: false
      }
    )
    (var-set next-vesting-schedule-id new-id)
    (print { event: "vesting-create", id: new-id, beneficiary: beneficiary, amount: total-amount })
    (ok new-id)
  )
)

(define-read-only (vested-amount
  (beneficiary principal)
  (schedule-id uint)
  (at-time uint))
  (match (map-get? vesting-schedules { beneficiary: beneficiary, schedule-id: schedule-id })
    vs
    (let (
      (start (get start-time vs))
      (cliff (get cliff-duration vs))
      (dur (get vesting-duration vs))
      (total (get total-amount vs))
    )
      (if (< at-time (+ start cliff))
        u0
        (if (>= at-time (+ start dur))
          total
          (/ (* total (- at-time start)) dur)
        )
      )
    )
    u0
  )
)

(define-public (release-vested (schedule-id uint))
  (match (map-get? vesting-schedules { beneficiary: tx-sender, schedule-id: schedule-id })
    vs
    (let (
      (rev (get revoked vs))
      (total (get total-amount vs))
      (released (get released-amount vs))
      (vested (vested-amount tx-sender schedule-id block-height))
    )
      (asserts! (not rev) ERR-VESTING-REVOKED)
      (let ((releasable (if (> vested released) (- vested released) u0)))
        (asserts! (> releasable u0) ERR-INSUFFICIENT-REWARDS)
        (try! (as-contract (ft-transfer? abtc-gov releasable (as-contract tx-sender) tx-sender)))
        (map-set vesting-schedules
          { beneficiary: tx-sender, schedule-id: schedule-id }
          (merge vs { released-amount: (+ released releasable) })
        )
        (print { event: "vesting-release", id: schedule-id, beneficiary: tx-sender, amount: releasable })
        (ok releasable)
      )
    )
    ERR-VESTING-NOT-FOUND
  )
)

(define-public (revoke-vesting (beneficiary principal) (schedule-id uint))
  (match (map-get? vesting-schedules { beneficiary: beneficiary, schedule-id: schedule-id })
    vs
    (begin
      (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
      (asserts! (get revocable vs) ERR-UNAUTHORIZED)
      (asserts! (not (get revoked vs)) ERR-VESTING-REVOKED)
      (map-set vesting-schedules
        { beneficiary: beneficiary, schedule-id: schedule-id }
        (merge vs { revoked: true })
      )
      (print { event: "vesting-revoke", id: schedule-id, beneficiary: beneficiary })
      (ok true)
    )
    ERR-VESTING-NOT-FOUND
  )
)

;; =========================
;; Administrative controls
;; =========================

(define-public (set-staking-enabled (enabled bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (var-set staking-enabled enabled)
    (print { event: "staking-enabled", value: enabled })
    (ok true)
  )
)

(define-public (set-delegation-enabled (enabled bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (var-set delegation-enabled enabled)
    (print { event: "delegation-enabled", value: enabled })
    (ok true)
  )
)

(define-public (set-unstake-cooldown (blocks uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (var-set unstake-cooldown-period blocks)
    (print { event: "cooldown-updated", value: blocks })
    (ok true)
  )
)

(define-public (set-min-stake (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (var-set min-stake-amount amount)
    (print { event: "min-stake-updated", value: amount })
    (ok true)
  )
)

(define-public (set-delegatee-commission (delegatee principal) (commission-bp uint))
  (let ((stats (default-to
    { total-delegated: u0, delegator-count: u0, commission-rate: u0, reputation-score: u100, total-commission-earned: u0 }
    (map-get? delegatee-stats { delegatee: delegatee })
  )))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (map-set delegatee-stats
      { delegatee: delegatee }
      (merge stats { commission-rate: commission-bp })
    )
    (print { event: "delegatee-commission", delegatee: delegatee, commission: commission-bp })
    (ok true)
  )
)

;; =========================
;; Administrative mint/burn
;; =========================

(define-public (mint (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (<= (+ (ft-get-supply abtc-gov) amount) TOKEN-MAX-SUPPLY) ERR-INVALID-AMOUNT)
    (try! (ft-mint? abtc-gov amount recipient))
    (print { event: "mint", recipient: recipient, amount: amount })
    (ok amount)
  )
)

(define-public (burn (amount uint) (owner principal))
  (begin
    (asserts! (or (is-eq tx-sender owner) (is-eq tx-sender CONTRACT-OWNER)) ERR-UNAUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (try! (ft-burn? abtc-gov amount owner))
    (print { event: "burn", owner: owner, amount: amount })
    (ok amount)
  )
)

;; =========================
;; Read-only helpers
;; =========================

(define-read-only (get-staking-info (user principal))
  (map-get? staker-info { staker: user })
)

(define-read-only (get-delegation-info (user principal))
  (map-get? delegation-info { delegator: user })
)

(define-read-only (get-delegatee-stats (delegatee principal))
  (map-get? delegatee-stats { delegatee: delegatee })
)

(define-read-only (get-pool-info (pool-id uint))
  (map-get? liquidity-pools { pool-id: pool-id })
)

(define-read-only (get-liquidity-position (user principal) (pool-id uint))
  (map-get? user-liquidity-positions { user: user, pool-id: pool-id })
)

(define-read-only (get-pool-counter)
  (var-get pool-counter)
)

(define-read-only (calculate-voting-power (user principal))
  (let (
    (balance (ft-get-balance abtc-gov user))
    (staking-info (map-get? staker-info { staker: user }))
    (delegation (map-get? delegation-info { delegator: user }))
    (staked-power (match staking-info
      stake-data (/ (* (get staked-amount stake-data) (get conviction-multiplier stake-data)) u100)
      u0))
    (delegated-power (match delegation
      del-data (get amount del-data)
      u0))
  )
    (+ balance staked-power (- delegated-power u0))
  )
)

;; =========================
;; Initial token distribution (deployment-time)
;; =========================

(ft-mint? abtc-gov u100000000000 tx-sender) ;; 100K initial tokens to deployer
