;; AutonomousBTC Vaults - Advanced Governance Token with DeFi Features
;; Enterprise-grade token with staking, delegation, yield farming, and advanced governance

(impl-trait 'SP3FBR2AGK5H9QBDH3EEN6DF8EK8JY7RX8QJ5SVTE.sip-010-trait-ft-standard.sip-010-trait)

;; Constants
(define-constant TOKEN-NAME "AutonomousBTC Governance Token")
(define-constant TOKEN-SYMBOL "ABTC-GOV")
(define-constant TOKEN-DECIMALS u6)
(define-constant TOKEN-MAX-SUPPLY u1000000000000) ;; 1M tokens with 6 decimals
(define-constant CONTRACT-OWNER tx-sender)

;; Staking and Yield Constants
(define-constant BASE-APY u800) ;; 8% base APY for staking
(define-constant MAX-STAKE-MULTIPLIER u300) ;; 3x max multiplier for long-term staking
(define-constant CONVICTION-PERIODS (list u144 u432 u1008 u2016 u4032)) ;; 1d, 3d, 7d, 14d, 28d
(define-constant CONVICTION-MULTIPLIERS (list u110 u125 u150 u200 u300)) ;; 1.1x, 1.25x, 1.5x, 2x, 3x

;; Error constants
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

;; Data variables
(define-data-var total-supply uint u0)
(define-data-var total-staked uint u0)
(define-data-var total-rewards-distributed uint u0)
(define-data-var reward-per-token-stored uint u0)
(define-data-var last-update-time uint u0)
(define-data-var staking-enabled bool true)
(define-data-var delegation-enabled bool true)
(define-data-var unstake-cooldown-period uint u432) ;; 3 days
(define-data-var min-stake-amount uint u100000) ;; 0.1 tokens minimum

;; Advanced token balances and metadata
(define-map token-balances principal uint)
(define-map token-allowances { owner: principal, spender: principal } uint)

;; Staking system maps
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
    commission-rate: uint, ;; Basis points (e.g., 500 = 5%)
    reputation-score: uint,
    total-commission-earned: uint
  }
)

;; Advanced yield farming maps
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

;; Governance enhancement maps
(define-map voting-escrow
  { user: principal }
  {
    locked-amount: uint,
    unlock-time: uint,
    voting-power: uint,
    lock-duration: uint
  }
)

(define-map governance-rewards
  { user: principal, proposal-id: uint }
  {
    reward-amount: uint,
    claimed: bool,
    participation-score: uint
  }
)

;; Token distribution and vesting
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

(define-map team-allocations
  { team-member: principal }
  {
    allocation: uint,
    vested: uint,
    cliff-passed: bool,
    vesting-start: uint
  }
)

;; SIP-010 Standard Functions
(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
  (begin
    (asserts! (or (is-eq tx-sender sender) (is-eq contract-call-sender sender)) ERR-UNAUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (let (
      (sender-balance (unwrap-panic (get-balance sender)))
    )
      (asserts! (>= sender-balance amount) ERR-INSUFFICIENT-BALANCE)
      (try! (ft-transfer? abtc-gov amount sender recipient))
      (print { event: "transfer", sender: sender, recipient: recipient, amount: amount, memo: memo })
      (ok true)
    )
  )
)

(define-read-only (get-name)
  (ok TOKEN-NAME)
)

(define-read-only (get-symbol)
  (ok TOKEN-SYMBOL)
)

(define-read-only (get-decimals)
  (ok TOKEN-DECIMALS)
)

(define-read-only (get-balance (who principal))
  (ok (ft-get-balance abtc-gov who))
)

(define-read-only (get-total-supply)
  (ok (ft-get-supply abtc-gov))
)

(define-read-only (get-token-uri)
  (ok (some u"https://autonomousbtc.vaults/metadata.json"))
)

;; Advanced Staking Functions
(define-public (stake-tokens (amount uint) (lock-period uint))
  (let (
    (user-balance (unwrap-panic (get-balance tx-sender)))
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
    
    ;; Update rewards before staking
    (try! (update-staking-rewards tx-sender))
    
    ;; Transfer tokens to contract
    (try! (ft-transfer? abtc-gov amount tx-sender (as-contract tx-sender)))
    
    ;; Update staker info
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
    
    ;; Update global staking stats
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
    
    ;; Update rewards before unstaking request
    (try! (update-staking-rewards tx-sender))
    
    ;; Set unstake request
    (map-set staker-info
      { staker: tx-sender }
      (merge current-stake {
        unstake-request-time: (some block-height)
      })
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
    
    ;; Update rewards before completing unstake
    (try! (update-staking-rewards tx-sender))
    
    ;; Transfer tokens back to user
    (try! (as-contract (ft-transfer? abtc-gov amount tx-sender tx-sender)))
    
    ;; Update staker info
    (map-set staker-info
      { staker: tx-sender }
      (merge current-stake {
        staked-amount: (- staked-amount amount),
        unstake-request-time: (if (is-eq (- staked-amount amount) u0) none (get unstake-request-time current-stake))
      })
    )
    
    ;; Update global stats
    (var-set total-staked (- (var-get total-staked) amount))
    
    (print { event: "unstake-complete", user: tx-sender, amount: amount })
    (ok amount)
  )
)

(define-public (claim-staking-rewards)
  (let (
    (current-stake (unwrap! (map-get? staker-info { staker: tx-sender }) ERR-INSUFFICIENT-BALANCE))
  )
    ;; Update rewards calculation
    (try! (update-staking-rewards tx-sender))
    
    (let (
      (updated-stake (unwrap! (map-get? staker-info { staker: tx-sender }) ERR-INSUFFICIENT-BALANCE))
      (rewards-to-claim (get rewards-earned updated-stake))
    )
      (asserts! (> rewards-to-claim u0) ERR-INSUFFICIENT-REWARDS)
      
      ;; Mint reward tokens
      (try! (ft-mint? abtc-gov rewards-to-claim tx-sender))
      
      ;; Update staker info
      (map-set staker-info
        { staker: tx-sender }
        (merge updated-stake {
          rewards-earned: u0,
          last-reward-claim: block-height,
          total-rewards-claimed: (+ (get total-rewards-claimed updated-stake) rewards-to-claim)
        })
      )
      
      ;; Update global stats
      (var-set total-rewards-distributed (+ (var-get total-rewards-distributed) rewards-to-claim))
      
      (print { event: "claim-rewards", user: tx-sender, amount: rewards-to-claim })
      (ok rewards-to-claim)
    )
  )
)

;; Advanced Delegation System
(define-public (delegate-voting-power (delegatee principal) (amount uint))
  (let (
    (delegator-balance (unwrap-panic (get-balance tx-sender)))
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
    
    ;; Create delegation
    (map-set delegation-info
      { delegator: tx-sender }
      {
        delegatee: delegatee,
        amount: amount,
        timestamp: block-height,
        active: true
      }
    )
    
    ;; Update delegatee stats
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
    (delegatee-stats-current (unwrap! (map-get? delegatee-stats { delegatee: delegatee }) ERR-UNAUTHORIZED))
  )
    ;; Remove delegation
    (map-delete delegation-info { delegator: tx-sender })
    
    ;; Update delegatee stats
    (map-set delegatee-stats
      { delegatee: delegatee }
      {
        total-delegated: (- (get total-delegated delegatee-stats-current) amount),
        delegator-count: (- (get delegator-count delegatee-stats-current) u1),
        commission-rate: (get commission-rate delegatee-stats-current),
        reputation-score: (get reputation-score delegatee-stats-current),
        total-commission-earned: (get total-commission-earned delegatee-stats-current)
      }
    )
    
    (print { event: "undelegate", delegator: tx-sender, delegatee: delegatee, amount: amount })
    (ok true)
  )
)

;; Yield Farming System
(define-public (create-liquidity-pool (token-pair (string-ascii 32)) (initial-apy uint))
  (let (
    (new-pool-id (+ (default-to u0 (get pool-id (unwrap-panic (element-at (keys liquidity-pools) (- (len (keys liquidity-pools)) u1))))) u1))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (> initial-apy u0) ERR-INVALID-AMOUNT)
    
    (map-set liquidity-pools
      { pool-id: new-pool-id }
      {
        token-pair: token-pair,
        total-liquidity: u0,
        apy: initial-apy,
        active: true,
        created-at: block-height,
        rewards-per-block: (/ initial-apy u52560) ;; Approximate blocks per year
      }
    )
    
    (print { event: "pool-created", pool-id: new-pool-id, token-pair: token-pair, apy: initial-apy })
    (ok new-pool-id)
  )
)

(define-public (provide-liquidity (pool-id uint) (amount uint))
  (let (
    (pool-info (unwrap! (map-get? liquidity-pools { pool-id: pool-id }) ERR-INVALID-AMOUNT))
    (user-balance (unwrap-panic (get-balance tx-sender)))
    (current-position (default-to
      { liquidity-amount: u0, rewards-earned: u0, entry-timestamp: block-height, last-claim: block-height }
      (map-get? user-liquidity-positions { user: tx-sender, pool-id: pool-id })
    ))
  )
    (asserts! (get active pool-info) ERR-UNAUTHORIZED)
    (asserts! (>= user-balance amount) ERR-INSUFFICIENT-BALANCE)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    
    ;; Transfer tokens to contract
    (try! (ft-transfer? abtc-gov amount tx-sender (as-contract tx-sender)))
    
    ;; Update user position
    (map-set user-liquidity-positions
      { user: tx-sender, pool-id: pool-id }
      {
        liquidity-amount: (+ (get liquidity-amount current-position) amount),
        rewards-earned: (get rewards-earned current-position),
        entry-timestamp: (get entry-timestamp current-position),
        last-claim: (get last-claim current-position)
      }
    )
    
    ;; Update pool stats
    (map-set liquidity-pools
      { pool-id: pool-id }
      (merge pool-info {
        total-liquidity: (+ (get total-liquidity pool-info) amount)
      })
    )
    
    (print { event: "liquidity-provided", user: tx-sender, pool-id: pool-id, amount: amount })
    (ok true)
  )
)

;; Administrative functions
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

;; Helper functions
(define-private (get-conviction-multiplier (lock-period uint))
  (let (
    (periods CONVICTION-PERIODS)
    (multipliers CONVICTION-MULTIPLIERS)
  )
    (if (>= lock-period u4032) u300   ;; 28+ days = 3x
      (if (>= lock-period u2016) u200   ;; 14+ days = 2x
        (if (>= lock-period u1008) u150   ;; 7+ days = 1.5x
          (if (>= lock-period u432) u125    ;; 3+ days = 1.25x
            (if (>= lock-period u144) u110    ;; 1+ day = 1.1x
              u100)))))  ;; Default = 1x
    )
)

(define-private (update-staking-rewards (user principal))
  (let (
    (current-stake (unwrap! (map-get? staker-info { staker: user }) (ok false)))
    (staked-amount (get staked-amount current-stake))
    (last-claim (get last-reward-claim current-stake))
    (time-diff (- block-height last-claim))
    (multiplier (get conviction-multiplier current-stake))
    (reward-rate (/ (* BASE-APY multiplier) u10000)) ;; Annual rate with multiplier
    (block-reward (/ (* staked-amount reward-rate) u52560)) ;; Approximate blocks per year
    (total-reward (* block-reward time-diff))
  )
    (if (> staked-amount u0)
      (map-set staker-info
        { staker: user }
        (merge current-stake {
          rewards-earned: (+ (get rewards-earned current-stake) total-reward)
        })
      )
      false
    )
    (ok true)
  )
)

;; Read-only helper functions
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

(define-read-only (calculate-voting-power (user principal))
  (let (
    (balance (ft-get-balance abtc-gov user))
    (staking-info (map-get? staker-info { staker: user }))
    (delegation-info (map-get? delegation-info { delegator: user }))
    (staked-power (match staking-info
      stake-data (/ (* (get staked-amount stake-data) (get conviction-multiplier stake-data)) u100)
      u0
    ))
    (delegated-power (match delegation-info
      del-data (get amount del-data)
      u0
    ))
  )
    (+ balance staked-power (- delegated-power u0))
  )
)

;; Initialize fungible token
(define-fungible-token abtc-gov)

;; Initial token distribution
(ft-mint? abtc-gov u100000000000 tx-sender) ;; 100K initial tokens to deployer
