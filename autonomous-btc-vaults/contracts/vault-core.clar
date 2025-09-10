;; AutonomousBTC Vaults - Core Contract
;; Enterprise-grade vault system with AI strategies, performance tracking, risk metrics, and advanced features

;; ============ CONSTANTS - SECURITY & ERROR CODES ============
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-VAULT-NOT-FOUND (err u100))
(define-constant ERR-INSUFFICIENT-BALANCE (err u101))
(define-constant ERR-UNAUTHORIZED (err u102))
(define-constant ERR-INVALID-STRATEGY (err u103))
(define-constant ERR-INVALID-AMOUNT (err u104))
(define-constant ERR-VAULT-PAUSED (err u105))
(define-constant ERR-VAULT-INACTIVE (err u106))
(define-constant ERR-SHARES-CALCULATION-ERROR (err u107))
(define-constant ERR-WITHDRAWAL-LIMIT-EXCEEDED (err u108))
(define-constant ERR-EMERGENCY-PAUSE-ACTIVE (err u109))
(define-constant ERR-INVALID-RECIPIENT (err u110))
(define-constant ERR-VAULT-LIMIT-REACHED (err u111))
(define-constant ERR-MINIMUM-DEPOSIT-NOT-MET (err u112))
(define-constant ERR-COOLDOWN-PERIOD-ACTIVE (err u113))
(define-constant ERR-SLIPPAGE-TOO-HIGH (err u114))
(define-constant ERR-INVALID-TIMEFRAME (err u115))
(define-constant ERR-EXECUTION-TOO-SOON (err u116))
(define-constant ERR-INVALID-HASH (err u117))
(define-constant ERR-INVALID-PARAMETERS (err u118))
(define-constant ERR-INVALID-CONFIDENCE (err u119))

;; ============ SYSTEM PARAMETERS ============
(define-constant MAX-VAULTS u10000)
(define-constant MIN-DEPOSIT-AMOUNT u1000000) ;; 1 STX minimum
(define-constant MAX-DEPOSIT-AMOUNT u100000000000) ;; 100K STX maximum
(define-constant WITHDRAWAL-COOLDOWN-PERIOD u144) ;; 24 hours in blocks
(define-constant MAX-WITHDRAWAL-PER-TX u10000000000) ;; 10K STX max per withdrawal
(define-constant PLATFORM-FEE-BASIS-POINTS u50) ;; 0.5% platform fee
(define-constant PERFORMANCE-FEE-BASIS-POINTS u200) ;; 2% performance fee
(define-constant EMERGENCY-WITHDRAWAL-FEE u500) ;; 5% emergency withdrawal fee
(define-constant SHARES-PRECISION u1000000) ;; 6 decimal precision for shares

;; ============ DATA VARIABLES - SYSTEM STATE ============
(define-data-var vault-counter uint u0)
(define-data-var total-value-locked uint u0)
(define-data-var contract-paused bool false)
(define-data-var emergency-mode bool false)
(define-data-var platform-fee uint PLATFORM-FEE-BASIS-POINTS)
(define-data-var performance-fee uint PERFORMANCE-FEE-BASIS-POINTS)
(define-data-var treasury-address principal tx-sender)
(define-data-var max-vaults-per-user uint u50)
(define-data-var global-performance-multiplier uint u100) ;; 1.0x default
(define-data-var risk-adjustment-factor uint u100) ;; 1.0x default
(define-data-var last-global-update uint u0)
(define-data-var total-fees-collected uint u0)
(define-data-var system-health-score uint u100) ;; 100% healthy

;; ============ DATA MAPS - VAULT CORE DATA ============
(define-map vaults
  { vault-id: uint }
  {
    creator: principal,
    name: (string-ascii 64),
    description: (string-ascii 256),
    total-deposited: uint,
    current-value: uint,
    strategy-id: uint,
    risk-level: uint, ;; 1-10 scale
    created-at: uint,
    last-update: uint,
    last-strategy-execution: uint,
    active: bool,
    emergency-pause: bool,
    vault-type: uint, ;; 1=conservative, 2=balanced, 3=aggressive, 4=custom
    minimum-deposit: uint,
    maximum-capacity: uint,
    withdrawal-fee: uint,
    management-fee: uint, ;; Annual management fee in basis points
    high-water-mark: uint, ;; For performance fee calculation
    inception-price: uint, ;; Initial share price
    total-deposits-count: uint,
    total-withdrawals-count: uint,
    total-fees-paid: uint,
    auto-compound: bool,
    social-trading: bool, ;; Allow others to copy this vault
    insurance-pool: uint ;; Insurance allocation
  }
)

(define-map user-balances
  { vault-id: uint, user: principal }
  {
    balance: uint, ;; Original deposit amount
    shares: uint, ;; Vault shares owned
    deposit-timestamp: uint,
    last-withdrawal: uint,
    total-deposited: uint, ;; Lifetime deposits
    total-withdrawn: uint, ;; Lifetime withdrawals
    realized-gains: int, ;; Realized P&L
    unrealized-gains: int, ;; Current unrealized P&L
    average-entry-price: uint, ;; Average entry price per share
    deposit-count: uint, ;; Number of deposits made
    withdrawal-count: uint, ;; Number of withdrawals made
    fees-paid: uint, ;; Total fees paid
    rewards-earned: uint, ;; Rewards from staking/farming
    last-reward-claim: uint, ;; Last reward claim timestamp
    risk-tolerance: uint, ;; User's risk tolerance 1-10
    auto-reinvest: bool, ;; Auto-reinvest profits
    stop-loss: uint, ;; Stop loss percentage (0 = disabled)
    take-profit: uint ;; Take profit percentage (0 = disabled)
  }
)

(define-map vault-performance
  { vault-id: uint }
  {
    total-value: uint,
    total-shares: uint,
    share-price: uint, ;; Price per share in STX
    apy: uint, ;; Annualized percentage yield
    apy-7d: uint, ;; 7-day APY
    apy-30d: uint, ;; 30-day APY
    volatility: uint, ;; Volatility measure
    sharpe-ratio: uint, ;; Risk-adjusted return metric
    max-drawdown: uint, ;; Maximum drawdown from peak
    last-update: uint,
    performance-score: uint, ;; Overall performance score 0-200
    total-return: int, ;; Total return since inception
    benchmark-outperformance: int, ;; Performance vs benchmark
    win-rate: uint, ;; Percentage of profitable periods
    profit-factor: uint, ;; Gross profit / gross loss
    calmar-ratio: uint, ;; Annual return / max drawdown
    sortino-ratio: uint, ;; Downside risk adjusted return
    daily-returns: (list 30 int), ;; Last 30 daily returns
    monthly-returns: (list 12 int), ;; Last 12 monthly returns
    best-day: int, ;; Best single day return
    worst-day: int, ;; Worst single day return
    consecutive-wins: uint, ;; Current winning streak
    consecutive-losses: uint ;; Current losing streak
  }
)

(define-map vault-strategies
  { vault-id: uint }
  {
    current-strategy: uint,
    strategy-name: (string-ascii 64),
    ai-recommendation-hash: (buff 32),
    last-execution: uint,
    execution-count: uint,
    successful-executions: uint,
    failed-executions: uint,
    total-ai-profit: int, ;; Total profit from AI strategies
    ai-confidence-avg: uint, ;; Average AI confidence level
    strategy-switches: uint, ;; Number of strategy changes
    last-strategy-switch: uint,
    ai-model-version: (string-ascii 32),
    strategy-parameters: (buff 512), ;; Strategy-specific parameters
    risk-parameters: (buff 256), ;; Risk management parameters
    execution-frequency: uint, ;; How often to execute (blocks)
    slippage-tolerance: uint, ;; Maximum acceptable slippage
    position-sizing: uint, ;; Position size as % of vault
    stop-loss-level: uint, ;; Strategy stop-loss level
    take-profit-level: uint, ;; Strategy take-profit level
    max-leverage: uint, ;; Maximum leverage allowed
    correlation-threshold: uint ;; Correlation threshold for diversification
  }
)

(define-map vault-analytics
  { vault-id: uint, period: uint } ;; period: 1=daily, 7=weekly, 30=monthly
  {
    start-value: uint,
    end-value: uint,
    high-value: uint,
    low-value: uint,
    volume: uint, ;; Trading volume
    trades-count: uint,
    profitable-trades: uint,
    loss-trades: uint,
    avg-trade-size: uint,
    avg-profit-per-trade: int,
    largest-win: int,
    largest-loss: int,
    recovery-time: uint, ;; Time to recover from drawdowns
    consistency-score: uint, ;; How consistent the returns are
    risk-score: uint, ;; Current risk level
    diversification-score: uint, ;; Portfolio diversification measure
    liquidity-score: uint, ;; How liquid the vault positions are
    efficiency-ratio: uint ;; Profit efficiency measure
  }
)

(define-map vault-risk-metrics
  { vault-id: uint }
  {
    var-95: uint, ;; Value at Risk at 95% confidence
    var-99: uint, ;; Value at Risk at 99% confidence
    expected-shortfall: uint, ;; Conditional VaR
    beta: uint, ;; Market beta coefficient
    alpha: uint, ;; Market alpha coefficient
    correlation-btc: int, ;; Correlation with Bitcoin (-100 to 100)
    correlation-stx: int, ;; Correlation with STX (-100 to 100)
    correlation-market: int, ;; Correlation with overall market
    tracking-error: uint, ;; Tracking error vs benchmark
    information-ratio: uint, ;; Information ratio metric
    downside-deviation: uint, ;; Downside volatility measure
    upside-capture: uint, ;; Upside capture ratio
    downside-capture: uint, ;; Downside capture ratio
    tail-ratio: uint, ;; Tail risk ratio
    skewness: int, ;; Return distribution skewness
    kurtosis: uint, ;; Return distribution kurtosis
    stress-test-score: uint, ;; Stress test performance
    liquidity-risk: uint, ;; Liquidity risk assessment
    concentration-risk: uint, ;; Concentration risk level
    operational-risk: uint ;; Operational risk score
  }
)

(define-map vault-limits
  { vault-id: uint }
  {
    max-single-deposit: uint,
    max-total-deposits: uint,
    max-withdrawal-per-day: uint,
    max-users: uint,
    min-balance-threshold: uint,
    emergency-exit-threshold: uint, ;; Auto-exit if losses exceed this
    rebalance-threshold: uint, ;; Trigger rebalancing
    max-drawdown-limit: uint, ;; Max acceptable drawdown
    concentration-limit: uint, ;; Max % in single position
    leverage-limit: uint, ;; Maximum leverage ratio
    correlation-limit: uint, ;; Max correlation between positions
    volatility-limit: uint, ;; Max acceptable volatility
    liquidity-requirement: uint, ;; Min liquidity % required
    geographic-exposure-limit: uint, ;; Geographic concentration limit
    sector-exposure-limit: uint, ;; Sector concentration limit
    counterparty-exposure-limit: uint, ;; Counterparty risk limit
    duration-limit: uint, ;; Max position duration
    frequency-limit: uint, ;; Max trading frequency
    slippage-limit: uint, ;; Max acceptable slippage
    spread-limit: uint ;; Max acceptable bid-ask spread
  }
)

(define-map vault-permissions
  { vault-id: uint, user: principal }
  {
    permission-level: uint, ;; 1=view, 2=deposit, 3=manage, 4=admin
    granted-by: principal,
    granted-at: uint,
    expires-at: (optional uint),
    can-deposit: bool,
    can-withdraw: bool,
    can-manage-strategy: bool,
    can-update-parameters: bool,
    can-add-users: bool,
    can-emergency-pause: bool,
    withdrawal-limit: uint,
    daily-limit: uint,
    monthly-limit: uint,
    requires-approval: bool,
    approval-threshold: uint, ;; Amount requiring approval
    multi-sig-required: bool,
    ip-whitelist: (list 5 (buff 32)), ;; IP address restrictions
    time-restrictions: (buff 168) ;; 24*7 hourly restrictions
  }
)

;; ============ READ-ONLY FUNCTIONS ============
(define-read-only (get-vault-info (vault-id uint))
  (map-get? vaults { vault-id: vault-id })
)

(define-read-only (get-user-balance (vault-id uint) (user principal))
  (map-get? user-balances { vault-id: vault-id, user: user })
)

(define-read-only (get-vault-performance (vault-id uint))
  (map-get? vault-performance { vault-id: vault-id })
)

(define-read-only (get-vault-strategy (vault-id uint))
  (map-get? vault-strategies { vault-id: vault-id })
)

(define-read-only (get-vault-analytics (vault-id uint) (period uint))
  (map-get? vault-analytics { vault-id: vault-id, period: period })
)

(define-read-only (get-vault-risk-metrics (vault-id uint))
  (map-get? vault-risk-metrics { vault-id: vault-id })
)

(define-read-only (get-vault-limits (vault-id uint))
  (map-get? vault-limits { vault-id: vault-id })
)

(define-read-only (get-vault-permissions (vault-id uint) (user principal))
  (map-get? vault-permissions { vault-id: vault-id, user: user })
)

(define-read-only (get-vault-counter)
  (var-get vault-counter)
)

(define-read-only (get-total-value-locked)
  (var-get total-value-locked)
)

(define-read-only (get-system-health-score)
  (var-get system-health-score)
)

(define-read-only (get-platform-statistics)
  {
    total-vaults: (var-get vault-counter),
    total-value-locked: (var-get total-value-locked),
    total-fees-collected: (var-get total-fees-collected),
    system-health-score: (var-get system-health-score),
    platform-fee: (var-get platform-fee),
    performance-fee: (var-get performance-fee),
    contract-paused: (var-get contract-paused),
    emergency-mode: (var-get emergency-mode),
    last-global-update: (var-get last-global-update)
  }
)

;; ============ ADVANCED CALCULATIONS ============
(define-read-only (calculate-user-share-value (vault-id uint) (user principal))
  (let (
    (user-data (unwrap! (get-user-balance vault-id user) (err u404)))
    (performance-data (unwrap! (get-vault-performance vault-id) (err u404)))
    (user-shares (get shares user-data))
    (total-shares (get total-shares performance-data))
    (total-value (get total-value performance-data))
  )
    (if (> total-shares u0)
      (ok (/ (* user-shares total-value) total-shares))
      (ok u0)
    )
  )
)

(define-read-only (calculate-share-price (vault-id uint))
  (let (
    (performance-data (unwrap! (get-vault-performance vault-id) (err u404)))
    (total-shares (get total-shares performance-data))
    (total-value (get total-value performance-data))
  )
    (if (> total-shares u0)
      (ok (/ (* total-value SHARES-PRECISION) total-shares))
      (ok SHARES-PRECISION) ;; Initial price = 1.0
    )
  )
)

(define-read-only (calculate-apy (vault-id uint) (period uint))
  (let (
    (vault-data (unwrap! (get-vault-info vault-id) (err u404)))
    (performance-data (unwrap! (get-vault-performance vault-id) (err u404)))
    (current-value (get current-value vault-data))
    (inception-price (get inception-price vault-data))
    (time-elapsed (- block-height (get created-at vault-data)))
    (periods-per-year (/ u525600 period)) ;; Approximate blocks per year / period
  )
    (if (and (> current-value u0) (> inception-price u0) (> time-elapsed u0))
      (let (
        (total-return (/ (* (- (to-int current-value) (to-int inception-price)) u10000) (to-int inception-price)))
        (periods-elapsed (/ time-elapsed period))
        (annualized-return (if (> periods-elapsed u0)
          (/ (* total-return periods-per-year) periods-elapsed)
          u0
        ))
      )
        (ok annualized-return)
      )
      (ok u0)
    )
  )
)

(define-read-only (calculate-volatility (vault-id uint))
  (let (
    (performance-data (unwrap! (get-vault-performance vault-id) (err u404)))
    (daily-returns (get daily-returns performance-data))
    (returns-count (len daily-returns))
  )
    (if (> returns-count u2)
      (let (
        (sum-returns (fold (lambda (acc r) (+ acc r)) daily-returns 0))
        (mean-return (/ sum-returns (to-int returns-count)))
        (squared-deviations (map (lambda (r) (* (- r mean-return) (- r mean-return))) daily-returns))
        (sum-deviations (fold (lambda (acc sd) (+ acc sd)) squared-deviations 0))
        (variance (/ sum-deviations (to-int (- returns-count u1))))
        (std-dev (sqrt variance))
      )
        (ok std-dev)
      )
      (ok u0)
    )
  )
)

(define-read-only (calculate-sharpe-ratio (vault-id uint))
  (let (
    (apy-result (calculate-apy vault-id u1))
    (volatility-result (calculate-volatility vault-id))
    (risk-free-rate u200) ;; 2% risk-free rate assumption
  )
    (match apy-result
      apy-value (match volatility-result
        volatility-value (if (> volatility-value u0)
          (ok (/ (* (- apy-value risk-free-rate) u100) volatility-value))
          (ok u0)
        )
        error-vol error-vol
      )
      error-apy error-apy
    )
  )
)

;; ============ PRIVATE HELPER FUNCTIONS ============
(define-private (calculate-shares-for-deposit (vault-id uint) (deposit-amount uint))
  (let (
    (share-price-result (calculate-share-price vault-id))
  )
    (match share-price-result
      share-price (ok (/ (* deposit-amount SHARES-PRECISION) share-price))
      error error
    )
  )
)

(define-private (calculate-deposit-fee (vault-id uint) (amount uint))
  (let (
    (vault-data (unwrap! (get-vault-info vault-id) (err u404)))
    (platform-fee-amount (/ (* amount (var-get platform-fee)) u10000))
    (vault-management-fee (/ (* amount (get management-fee vault-data)) u10000))
  )
    (ok (+ platform-fee-amount vault-management-fee))
  )
)

(define-private (calculate-withdrawal-fee (vault-id uint) (amount uint) (emergency bool))
  (let (
    (vault-data (unwrap! (get-vault-info vault-id) (err u404)))
    (base-fee (get withdrawal-fee vault-data))
    (emergency-fee (if emergency EMERGENCY-WITHDRAWAL-FEE u0))
    (total-fee-rate (+ base-fee emergency-fee))
    (fee-amount (/ (* amount total-fee-rate) u10000))
  )
    (ok fee-amount)
  )
)

(define-private (update-vault-performance (vault-id uint) (new-value uint))
  (let (
    (vault-data (unwrap! (get-vault-info vault-id) (err u404)))
    (performance-data (unwrap! (get-vault-performance vault-id) (err u404)))
    (old-value (get current-value vault-data))
    (time-diff (- block-height (get last-update performance-data)))
    (daily-return (if (> old-value u0)
      (/ (* (- (to-int new-value) (to-int old-value)) 10000) (to-int old-value))
      0
    ))
    (current-returns (get daily-returns performance-data))
    (updated-returns (unwrap! (as-max-len? (append current-returns daily-return) u30) (err u500)))
    (new-share-price (if (> (get total-shares performance-data) u0)
      (/ (* new-value SHARES-PRECISION) (get total-shares performance-data))
      SHARES-PRECISION
    ))
  )
    (map-set vault-performance
      { vault-id: vault-id }
      (merge performance-data {
        total-value: new-value,
        share-price: new-share-price,
        last-update: block-height,
        daily-returns: updated-returns,
        best-day: (if (> daily-return (get best-day performance-data)) 
          daily-return 
          (get best-day performance-data)),
        worst-day: (if (< daily-return (get worst-day performance-data)) 
          daily-return 
          (get worst-day performance-data))
      })
    )
    (ok true)
  )
)

(define-private (validate-vault-limits (vault-id uint) (operation-type uint) (amount uint))
  (let (
    (limits (unwrap! (get-vault-limits vault-id) (ok true))) ;; No limits set = no restrictions
    (vault-data (unwrap! (get-vault-info vault-id) (err u404)))
  )
    (asserts! (<= amount (get max-single-deposit limits)) ERR-INVALID-AMOUNT)
    (asserts! (<= (+ (get current-value vault-data) amount) (get max-total-deposits limits)) ERR-VAULT-LIMIT-REACHED)
    (ok true)
  )
)

(define-private (check-user-permissions (vault-id uint) (user principal) (operation-type uint))
  (let (
    (permissions (map-get? vault-permissions { vault-id: vault-id, user: user }))
  )
    (match permissions
      perm-data (begin
        (asserts! (>= (get permission-level perm-data) operation-type) ERR-UNAUTHORIZED)
        (match (get expires-at perm-data)
          expiry (asserts! (< block-height expiry) ERR-UNAUTHORIZED)
          (ok true)
        )
      )
      (ok true) ;; No permissions set = vault creator has full access
    )
  )
)

(define-private (sqrt (n uint))
  (let (
    (guess (/ n u2))
    (iter1 (/ (+ guess (/ n guess)) u2))
    (iter2 (/ (+ iter1 (/ n iter1)) u2))
    (iter3 (/ (+ iter2 (/ n iter2)) u2))
    (iter4 (/ (+ iter3 (/ n iter3)) u2))
    (iter5 (/ (+ iter4 (/ n iter4)) u2))
    (iter6 (/ (+ iter5 (/ n iter5)) u2))
    (iter7 (/ (+ iter6 (/ n iter6)) u2))
    (iter8 (/ (+ iter7 (/ n iter7)) u2))
    (iter9 (/ (+ iter8 (/ n iter8)) u2))
    (iter10 (/ (+ iter9 (/ n iter9)) u2))
  )
    iter10
  )
)

(define-private (buff-to-uint-be (buffer (buff 4)))
  (let (
    (byte0 (unwrap! (element-at buffer u0) u0))
    (byte1 (unwrap! (element-at buffer u1) u0))
    (byte2 (unwrap! (element-at buffer u2) u0))
    (byte3 (unwrap! (element-at buffer u3) u0))
  )
    (+ 
      (* (buff-to-uint-le (list byte0)) u16777216)
      (* (buff-to-uint-le (list byte1)) u65536)
      (* (buff-to-uint-le (list byte2)) u256)
      (buff-to-uint-le (list byte3))
    )
  )
)

(define-private (max (a int) (b int))
  (if (> a b) a b)
)

(define-private (min (a uint) (b uint))
  (if (< a b) a b)
)

;; ============ PUBLIC FUNCTIONS ============
(define-public (create-vault 
  (name (string-ascii 64))
  (description (string-ascii 256))
  (strategy-id uint) 
  (initial-deposit uint)
  (vault-type uint)
  (risk-level uint))
  (let (
    (new-vault-id (+ (var-get vault-counter) u1))
    (current-height block-height)
  )
    (asserts! (not (var-get contract-paused)) ERR-VAULT-PAUSED)
    (asserts! (not (var-get emergency-mode)) ERR-EMERGENCY-PAUSE-ACTIVE)
    (asserts! (<= new-vault-id MAX-VAULTS) ERR-VAULT-LIMIT-REACHED)
    (asserts! (>= initial-deposit MIN-DEPOSIT-AMOUNT) ERR-MINIMUM-DEPOSIT-NOT-MET)
    (asserts! (<= initial-deposit MAX-DEPOSIT-AMOUNT) ERR-INVALID-AMOUNT)
    (asserts! (>= (stx-get-balance tx-sender) initial-deposit) ERR-INSUFFICIENT-BALANCE)
    (asserts! (and (>= vault-type u1) (<= vault-type u4)) ERR-INVALID-STRATEGY)
    (asserts! (and (>= risk-level u1) (<= risk-level u10)) ERR-INVALID-STRATEGY)
    (asserts! (> (len name) u0) ERR-INVALID-AMOUNT)
    
    (let (
      (fee-result (calculate-deposit-fee new-vault-id initial-deposit))
      (net-deposit (- initial-deposit (unwrap! fee-result ERR-INVALID-AMOUNT)))
    )
      (try! (stx-transfer? initial-deposit tx-sender (as-contract tx-sender)))
      (try! (as-contract (stx-transfer? (unwrap! fee-result ERR-INVALID-AMOUNT) tx-sender (var-get treasury-address))))
      
      (map-set vaults
        { vault-id: new-vault-id }
        {
          creator: tx-sender,
          name: name,
          description: description,
          total-deposited: net-deposit,
          current-value: net-deposit,
          strategy-id: strategy-id,
          risk-level: risk-level,
          created-at: current-height,
          last-update: current-height,
          last-strategy-execution: u0,
          active: true,
          emergency-pause: false,
          vault-type: vault-type,
          minimum-deposit: MIN-DEPOSIT-AMOUNT,
          maximum-capacity: MAX-DEPOSIT-AMOUNT,
          withdrawal-fee: u100,
          management-fee: u200,
          high-water-mark: net-deposit,
          inception-price: SHARES-PRECISION,
          total-deposits-count: u1,
          total-withdrawals-count: u0,
          total-fees-paid: (unwrap! fee-result ERR-INVALID-AMOUNT),
          auto-compound: true,
          social-trading: false,
          insurance-pool: (/ net-deposit u100)
        }
      )
      
      (map-set user-balances
        { vault-id: new-vault-id, user: tx-sender }
        {
          balance: net-deposit,
          shares: net-deposit,
          deposit-timestamp: current-height,
          last-withdrawal: u0,
          total-deposited: net-deposit,
          total-withdrawn: u0,
          realized-gains: 0,
          unrealized-gains: 0,
          average-entry-price: SHARES-PRECISION,
          deposit-count: u1,
          withdrawal-count: u0,
          fees-paid: (unwrap! fee-result ERR-INVALID-AMOUNT),
          rewards-earned: u0,
          last-reward-claim: current-height,
          risk-tolerance: risk-level,
          auto-reinvest: true,
          stop-loss: u0,
          take-profit: u0
        }
      )
      
      (map-set vault-performance
        { vault-id: new-vault-id }
        {
          total-value: net-deposit,
          total-shares: net-deposit,
          share-price: SHARES-PRECISION,
          apy: u0,
          apy-7d: u0,
          apy-30d: u0,
          volatility: u0,
          sharpe-ratio: u0,
          max-drawdown: u0,
          last-update: current-height,
          performance-score: u100,
          total-return: 0,
          benchmark-outperformance: 0,
          win-rate: u0,
          profit-factor: u0,
          calmar-ratio: u0,
          sortino-ratio: u0,
          daily-returns: (list),
          monthly-returns: (list),
          best-day: 0,
          worst-day: 0,
          consecutive-wins: u0,
          consecutive-losses: u0
        }
      )
      
      (map-set vault-strategies
        { vault-id: new-vault-id }
        {
          current-strategy: strategy-id,
          strategy-name: "Default Strategy",
          ai-recommendation-hash: 0x00,
          last-execution: u0,
          execution-count: u0,
          successful-executions: u0,
          failed-executions: u0,
          total-ai-profit: 0,
          ai-confidence-avg: u0,
          strategy-switches: u0,
          last-strategy-switch: current-height,
          ai-model-version: "v1.0",
          strategy-parameters: 0x00,
          risk-parameters: 0x00,
          execution-frequency: u144,
          slippage-tolerance: u100,
          position-sizing: u8000,
          stop-loss-level: u500,
          take-profit-level: u2000,
          max-leverage: u100,
          correlation-threshold: u7000
        }
      )
      
      (map-set vault-risk-metrics
        { vault-id: new-vault-id }
        {
          var-95: u0,
          var-99: u0,
          expected-shortfall: u0,
          beta: u100,
          alpha: u0,
          correlation-btc: 50,
          correlation-stx: 80,
          correlation-market: 60,
          tracking-error: u0,
          information-ratio: u0,
          downside-deviation: u0,
          upside-capture: u100,
          downside-capture: u100,
          tail-ratio: u100,
          skewness: 0,
          kurtosis: u300,
          stress-test-score: u100,
          liquidity-risk: u10,
          concentration-risk: u0,
          operational-risk: u5
        }
      )
      
      (map-set vault-limits
        { vault-id: new-vault-id }
        {
          max-single-deposit: MAX-DEPOSIT-AMOUNT,
          max-total-deposits: u1000000000000,
          max-withdrawal-per-day: MAX-WITHDRAWAL-PER-TX,
          max-users: u1000,
          min-balance-threshold: u100000,
          emergency-exit-threshold: u5000,
          rebalance-threshold: u500,
          max-drawdown-limit: u2000,
          concentration-limit: u2000,
          leverage-limit: u200,
          correlation-limit: u8000,
          volatility-limit: u5000,
          liquidity-requirement: u1000,
          geographic-exposure-limit: u5000,
          sector-exposure-limit: u3000,
          counterparty-exposure-limit: u2000,
          duration-limit: u2592000,
          frequency-limit: u10,
          slippage-limit: u200,
          spread-limit: u100
        }
      )
      
      (var-set vault-counter new-vault-id)
      (var-set total-value-locked (+ (var-get total-value-locked) net-deposit))
      (var-set total-fees-collected (+ (var-get total-fees-collected) (unwrap! fee-result ERR-INVALID-AMOUNT)))
      (var-set last-global-update current-height)
      
      (ok new-vault-id)
    )
  )
)

(define-public (deposit-to-vault (vault-id uint) (amount uint) (min-shares-expected uint))
  (let (
    (vault-data (unwrap! (get-vault-info vault-id) ERR-VAULT-NOT-FOUND))
    (current-user-balance (default-to
      { balance: u0, shares: u0, deposit-timestamp: u0, last-withdrawal: u0, total-deposited: u0, 
        total-withdrawn: u0, realized-gains: 0, unrealized-gains: 0, average-entry-price: SHARES-PRECISION,
        deposit-count: u0, withdrawal-count: u0, fees-paid: u0, rewards-earned: u0, last-reward-claim: u0,
        risk-tolerance: u5, auto-reinvest: true, stop-loss: u0, take-profit: u0 }
      (get-user-balance vault-id tx-sender)
    ))
    (performance-data (unwrap! (get-vault-performance vault-id) ERR-VAULT-NOT-FOUND))
    (current-height block-height)
  )
    (asserts! (not (var-get contract-paused)) ERR-VAULT-PAUSED)
    (asserts! (not (var-get emergency-mode)) ERR-EMERGENCY-PAUSE-ACTIVE)
    (asserts! (not (get emergency-pause vault-data)) ERR-VAULT-PAUSED)
    (asserts! (get active vault-data) ERR-VAULT-INACTIVE)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (>= amount (get minimum-deposit vault-data)) ERR-MINIMUM-DEPOSIT-NOT-MET)
    (asserts! (<= amount MAX-DEPOSIT-AMOUNT) ERR-INVALID-AMOUNT)
    (asserts! (>= (stx-get-balance tx-sender) amount) ERR-INSUFFICIENT-BALANCE)
    
    (asserts! (<= (+ (get current-value vault-data) amount) (get maximum-capacity vault-data)) ERR-VAULT-LIMIT-REACHED)
    
    (try! (check-user-permissions vault-id tx-sender u2))
    
    (try! (validate-vault-limits vault-id u1 amount))
    
    (let (
      (platform-fee-amount (/ (* amount (var-get platform-fee)) u10000))
      (vault-management-fee (/ (* amount (get management-fee vault-data)) u10000))
      (insurance-fee (/ (* amount u50) u10000))
      (total-fees (+ platform-fee-amount vault-management-fee insurance-fee))
      (net-deposit (- amount total-fees))
      (current-share-price (get share-price performance-data))
      (shares-to-issue (if (> current-share-price u0)
        (/ (* net-deposit SHARES-PRECISION) current-share-price)
        net-deposit
      ))
    )
      (asserts! (>= shares-to-issue min-shares-expected) ERR-SLIPPAGE-TOO-HIGH)
      (asserts! (> shares-to-issue u0) ERR-SHARES-CALCULATION-ERROR)
      
      (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
      (try! (as-contract (stx-transfer? platform-fee-amount tx-sender (var-get treasury-address))))
      (try! (as-contract (stx-transfer? insurance-fee tx-sender tx-sender)))
      
      (map-set vaults
        { vault-id: vault-id }
        (merge vault-data {
          total-deposited: (+ (get total-deposited vault-data) net-deposit),
          current-value: (+ (get current-value vault-data) net-deposit),
          last-update: current-height,
          total-deposits-count: (+ (get total-deposits-count vault-data) u1),
          total-fees-paid: (+ (get total-fees-paid vault-data) total-fees),
          insurance-pool: (+ (get insurance-pool vault-data) insurance-fee)
        })
      )
      
      (let (
        (old-total-cost (* (get shares current-user-balance) (get average-entry-price current-user-balance)))
        (new-total-cost (+ old-total-cost (* shares-to-issue current-share-price)))
        (new-total-shares (+ (get shares current-user-balance) shares-to-issue))
        (new-avg-entry-price (if (> new-total-shares u0)
          (/ new-total-cost new-total-shares)
          current-share-price
        ))
      )
        (map-set user-balances
          { vault-id: vault-id, user: tx-sender }
          {
            balance: (+ (get balance current-user-balance) net-deposit),
            shares: new-total-shares,
            deposit-timestamp: current-height,
            last-withdrawal: (get last-withdrawal current-user-balance),
            total-deposited: (+ (get total-deposited current-user-balance) net-deposit),
            total-withdrawn: (get total-withdrawn current-user-balance),
            realized-gains: (get realized-gains current-user-balance),
            unrealized-gains: (to-int (- (* new-total-shares current-share-price) new-total-cost)),
            average-entry-price: new-avg-entry-price,
            deposit-count: (+ (get deposit-count current-user-balance) u1),
            withdrawal-count: (get withdrawal-count current-user-balance),
            fees-paid: (+ (get fees-paid current-user-balance) total-fees),
            rewards-earned: (get rewards-earned current-user-balance),
            last-reward-claim: (get last-reward-claim current-user-balance),
            risk-tolerance: (get risk-tolerance current-user-balance),
            auto-reinvest: (get auto-reinvest current-user-balance),
            stop-loss: (get stop-loss current-user-balance),
            take-profit: (get take-profit current-user-balance)
          }
        )
      )
      
      (map-set vault-performance
        { vault-id: vault-id }
        (merge performance-data {
          total-value: (+ (get total-value performance-data) net-deposit),
          total-shares: (+ (get total-shares performance-data) shares-to-issue),
          last-update: current-height
        })
      )
      
      (var-set total-value-locked (+ (var-get total-value-locked) net-deposit))
      (var-set total-fees-collected (+ (var-get total-fees-collected) total-fees))
      (var-set last-global-update current-height)
      
      (let (
        (limits (unwrap! (get-vault-limits vault-id) (ok shares-to-issue)))
        (current-total (+ (get current-value vault-data) net-deposit))
        (rebalance-threshold (get rebalance-threshold limits))
        (last-rebalance-value (get high-water-mark vault-data))
      )
        (if (> (/ (* (abs (to-int (- current-total last-rebalance-value))) u10000) (to-int last-rebalance-value)) rebalance-threshold)
          (map-set vault-strategies
            { vault-id: vault-id }
            (merge (unwrap! (get-vault-strategy vault-id) (err u404)) {
              strategy-parameters: 0x01
            })
          )
          (ok true)
        )
      )
      
      (ok shares-to-issue)
    )
  )
)

(define-public (withdraw-from-vault (vault-id uint) (shares-to-redeem uint) (min-amount-expected uint) (emergency-withdrawal bool))
  (let (
    (vault-data (unwrap! (get-vault-info vault-id) ERR-VAULT-NOT-FOUND))
    (user-data (unwrap! (get-user-balance vault-id tx-sender) ERR-INSUFFICIENT-BALANCE))
    (performance-data (unwrap! (get-vault-performance vault-id) ERR-VAULT-NOT-FOUND))
    (limits (unwrap! (get-vault-limits vault-id) ERR-VAULT-NOT-FOUND))
    (current-height block-height)
    (user-shares (get shares user-data))
    (total-shares (get total-shares performance-data))
    (total-value (get total-value performance-data))
    (current-share-price (get share-price performance-data))
  )
    (asserts! (not (var-get contract-paused)) ERR-VAULT-PAUSED)
    (asserts! (get active vault-data) ERR-VAULT-INACTIVE)
    (asserts! (> shares-to-redeem u0) ERR-INVALID-AMOUNT)
    (asserts! (<= shares-to-redeem user-shares) ERR-INSUFFICIENT-BALANCE)
    (asserts! (> total-shares u0) ERR-SHARES-CALCULATION-ERROR)
    
    (if (not emergency-withdrawal)
      (asserts! (>= (- current-height (get last-withdrawal user-data)) WITHDRAWAL-COOLDOWN-PERIOD) ERR-COOLDOWN-PERIOD-ACTIVE)
      (ok true)
    )
    
    (let (
      (max-daily-withdrawal (get max-withdrawal-per-day limits))
      (withdrawal-amount-estimate (/ (* shares-to-redeem total-value) total-shares))
    )
      (asserts! (<= withdrawal-amount-estimate max-daily-withdrawal) ERR-WITHDRAWAL-LIMIT-EXCEEDED)
    )
    
    (let (
      (gross-withdrawal-amount (/ (* shares-to-redeem total-value) total-shares))
      (withdrawal-fee-result (calculate-withdrawal-fee vault-id gross-withdrawal-amount emergency-withdrawal))
      (withdrawal-fee (unwrap! withdrawal-fee-result ERR-INVALID-AMOUNT))
      (net-withdrawal-amount (- gross-withdrawal-amount withdrawal-fee))
    )
      (asserts! (>= net-withdrawal-amount min-amount-expected) ERR-SLIPPAGE-TOO-HIGH)
      (asserts! (>= (as-contract (stx-get-balance tx-sender)) gross-withdrawal-amount) ERR-INSUFFICIENT-BALANCE)
      
      (let (
        (cost-basis (* shares-to-redeem (get average-entry-price user-data)))
        (current-value-of-shares (* shares-to-redeem current-share-price))
        (realized-gain-loss (- (to-int current-value-of-shares) (to-int cost-basis)))
        (remaining-shares (- user-shares shares-to-redeem))
      )
        (try! (as-contract (stx-transfer? net-withdrawal-amount tx-sender tx-sender)))
        (try! (as-contract (stx-transfer? withdrawal-fee tx-sender (var-get treasury-address))))
        
        (map-set user-balances
          { vault-id: vault-id, user: tx-sender }
          (merge user-data {
            balance: (if (> (get balance user-data) net-withdrawal-amount)
              (- (get balance user-data) net-withdrawal-amount)
              u0
            ),
            shares: remaining-shares,
            last-withdrawal: current-height,
            total-withdrawn: (+ (get total-withdrawn user-data) net-withdrawal-amount),
            realized-gains: (+ (get realized-gains user-data) realized-gain-loss),
            unrealized-gains: (if (> remaining-shares u0)
              (to-int (- (* remaining-shares current-share-price) (* remaining-shares (get average-entry-price user-data))))
              0
            ),
            withdrawal-count: (+ (get withdrawal-count user-data) u1),
            fees-paid: (+ (get fees-paid user-data) withdrawal-fee)
          })
        )
        
        (map-set vaults
          { vault-id: vault-id }
          (merge vault-data {
            current-value: (- (get current-value vault-data) gross-withdrawal-amount),
            last-update: current-height,
            total-withdrawals-count: (+ (get total-withdrawals-count vault-data) u1),
            total-fees-paid: (+ (get total-fees-paid vault-data) withdrawal-fee)
          })
        )
        
        (map-set vault-performance
          { vault-id: vault-id }
          (merge performance-data {
            total-value: (- total-value gross-withdrawal-amount),
            total-shares: (- total-shares shares-to-redeem),
            last-update: current-height
          })
        )
        
        (var-set total-value-locked (- (var-get total-value-locked) gross-withdrawal-amount))
        (var-set total-fees-collected (+ (var-get total-fees-collected) withdrawal-fee))
        (var-set last-global-update current-height)
        
        (let (
          (remaining-vault-value (- (get current-value vault-data) gross-withdrawal-amount))
          (min-threshold (get min-balance-threshold limits))
        )
          (if (< remaining-vault-value min-threshold)
            (map-set vaults
              { vault-id: vault-id }
              (merge vault-data {
                emergency-pause: true
              })
            )
            (ok true)
          )
        )
        
        (ok net-withdrawal-amount)
      )
    )
  )
)

(define-public (execute-ai-strategy (vault-id uint) (ai-data (buff 512)) (strategy-params (buff 256)) (risk-params (buff 128)))
  (let (
    (vault-data (unwrap! (get-vault-info vault-id) ERR-VAULT-NOT-FOUND))
    (strategy-data (unwrap! (get-vault-strategy vault-id) ERR-VAULT-NOT-FOUND))
    (performance-data (unwrap! (get-vault-performance vault-id) ERR-VAULT-NOT-FOUND))
    (risk-metrics (unwrap! (get-vault-risk-metrics vault-id) ERR-VAULT-NOT-FOUND))
    (limits (unwrap! (get-vault-limits vault-id) ERR-VAULT-NOT-FOUND))
    (ai-hash (sha256 ai-data))
    (current-height block-height)
  )
    (asserts! (not (var-get contract-paused)) ERR-VAULT-PAUSED)
    (asserts! (not (var-get emergency-mode)) ERR-EMERGENCY-PAUSE-ACTIVE)
    (asserts! (not (get emergency-pause vault-data)) ERR-VAULT-PAUSED)
    (asserts! (get active vault-data) ERR-VAULT-INACTIVE)
    (asserts! (or (is-eq tx-sender (get creator vault-data)) (is-eq tx-sender CONTRACT-OWNER)) ERR-UNAUTHORIZED)
    
    (let (
      (last-execution (get last-execution strategy-data))
      (min-interval (get execution-frequency strategy-data))
      (time-since-last (- current-height last-execution))
    )
      (asserts! (>= time-since-last min-interval) ERR-EXECUTION-TOO-SOON)
    )
    
    (asserts! (> (len ai-data) u0) ERR-INVALID-HASH)
    (asserts! (> (len strategy-params) u0) ERR-INVALID-PARAMETERS)
    
    (let (
      (current-volatility (get volatility risk-metrics))
      (max-volatility (get volatility-limit limits))
      (current-drawdown (get max-drawdown performance-data))
      (max-drawdown-limit (get max-drawdown-limit limits))
    )
      (asserts! (<= current-volatility max-volatility) ERR-INVALID-STRATEGY)
      (asserts! (<= current-drawdown max-drawdown-limit) ERR-INVALID-STRATEGY)
    )
    
    (let (
      (ai-confidence (buff-to-uint-be (unwrap! (slice? ai-data u0 u4) (err u404))))
      (min-confidence-threshold u7000)
    )
      (asserts! (>= ai-confidence min-confidence-threshold) ERR-INVALID-CONFIDENCE)
      
      (map-set vault-strategies
        { vault-id: vault-id }
        (merge strategy-data {
          ai-recommendation-hash: ai-hash,
          last-execution: current-height,
          execution-count: (+ (get execution-count strategy-data) u1),
          strategy-parameters: strategy-params,
          risk-parameters: risk-params,
          ai-confidence-avg: (/ (+ (* (get ai-confidence-avg strategy-data) (get execution-count strategy-data)) ai-confidence) (+ (get execution-count strategy-data) u1))
        })
      )
      
      (map-set vaults
        { vault-id: vault-id }
        (merge vault-data {
          last-update: current-height,
          last-strategy-execution: current-height
        })
      )
      
      (map-set vault-analytics
        { vault-id: vault-id, period: current-height }
        {
          start-value: (get current-value vault-data),
          end-value: (get current-value vault-data),
          high-value: (get current-value vault-data),
          low-value: (get current-value vault-data),
          volume: u0,
          trades-count: u0,
          profitable-trades: u0,
          loss-trades: u0,
          avg-trade-size: u0,
          avg-profit-per-trade: 0,
          largest-win: 0,
          largest-loss: 0,
          recovery-time: u0,
          consistency-score: u0,
          risk-score: u0,
          diversification-score: u0,
          liquidity-score: u0,
          efficiency-ratio: u0
        }
      )
      
      (ok true)
    )
  )
)

(define-public (update-vault-value (vault-id uint) (new-value uint) (performance-attribution (buff 256)))
  (let (
    (vault-data (unwrap! (get-vault-info vault-id) ERR-VAULT-NOT-FOUND))
    (performance-data (unwrap! (get-vault-performance vault-id) ERR-VAULT-NOT-FOUND))
    (strategy-data (unwrap! (get-vault-strategy vault-id) ERR-VAULT-NOT-FOUND))
    (risk-metrics (unwrap! (get-vault-risk-metrics vault-id) ERR-VAULT-NOT-FOUND))
    (old-value (get current-value vault-data))
    (current-height block-height)
  )
    (asserts! (not (var-get contract-paused)) ERR-VAULT-PAUSED)
    (asserts! (get active vault-data) ERR-VAULT-INACTIVE)
    (asserts! (or (is-eq tx-sender (get creator vault-data)) (is-eq tx-sender CONTRACT-OWNER)) ERR-UNAUTHORIZED)
    (asserts! (> new-value u0) ERR-INVALID-AMOUNT)
    
    (let (
      (time-diff (- current-height (get last-update performance-data)))
      (value-change (- (to-int new-value) (to-int old-value)))
      (percentage-change (if (> old-value u0)
        (/ (* value-change 10000) (to-int old-value))
        0
      ))
      (is-profit (> value-change 0))
      (new-high-water-mark (if (> new-value (get high-water-mark vault-data))
        new-value
        (get high-water-mark vault-data)
      ))
      (drawdown-from-peak (if (> (get high-water-mark vault-data) new-value)
        (/ (* (- (get high-water-mark vault-data) new-value) u10000) (get high-water-mark vault-data))
        u0
      ))
    )
      (map-set vaults
        { vault-id: vault-id }
        (merge vault-data {
          current-value: new-value,
          last-update: current-height,
          high-water-mark: new-high-water-mark
        })
      )
      
      (let (
        (total-shares (get total-shares performance-data))
        (new-share-price (if (> total-shares u0)
          (/ (* new-value SHARES-PRECISION) total-shares)
          SHARES-PRECISION
        ))
        (current-returns (get daily-returns performance-data))
        (updated-returns (unwrap! (as-max-len? (append current-returns percentage-change) u30) (err u500)))
        (returns-count (len updated-returns))
        (volatility (if (> returns-count u2)
          (let (
            (sum-returns (fold (lambda (acc r) (+ acc r)) updated-returns 0))
            (mean-return (/ sum-returns (to-int returns-count)))
            (squared-deviations (map (lambda (r) (* (- r mean-return) (- r mean-return))) updated-returns))
            (sum-deviations (fold (lambda (acc sd) (+ acc sd)) squared-deviations 0))
            (variance (/ sum-deviations (to-int (- returns-count u1))))
          )
            (sqrt variance)
          )
          u0
        ))
        (inception-time (get created-at vault-data))
        (total-time-elapsed (- current-height inception-time))
        (inception-value (get inception-price vault-data))
        (total-return (if (> inception-value u0)
          (/ (* (- (to-int new-value) (to-int inception-value)) 10000) (to-int inception-value))
          0
        ))
        (apy (if (and (> total-time-elapsed u0) (> inception-value u0))
          (let (
            (years-elapsed (/ total-time-elapsed u525600))
            (annual-factor (if (> years-elapsed u0) years-elapsed u1))
          )
            (to-uint (/ (* total-return (to-int annual-factor)) (to-int years-elapsed)))
          )
          u0
        ))
      )
        (map-set vault-performance
          { vault-id: vault-id }
          (merge performance-data {
            total-value: new-value,
            share-price: new-share-price,
            apy: apy,
            volatility: volatility,
            max-drawdown: (max (get max-drawdown performance-data) drawdown-from-peak),
            last-update: current-height,
            performance-score: (if is-profit
              (min (+ (get performance-score performance-data) u5) u200)
              (max (- (get performance-score performance-data) u3) u0)
            ),
            total-return: total-return,
            daily-returns: updated-returns,
            best-day: (max (get best-day performance-data) percentage-change),
            worst-day: (min (get worst-day performance-data) percentage-change),
            consecutive-wins: (if is-profit 
              (+ (get consecutive-wins performance-data) u1)
              u0
            ),
            consecutive-losses: (if (not is-profit)
              (+ (get consecutive-losses performance-data) u1)
              u0
            )
          })
        )
      )
      
      (let (
        (execution-successful (>= percentage-change 0))
      )
        (map-set vault-strategies
          { vault-id: vault-id }
          (merge strategy-data {
            successful-executions: (if execution-successful
              (+ (get successful-executions strategy-data) u1)
              (get successful-executions strategy-data)
            ),
            failed-executions: (if (not execution-successful)
              (+ (get failed-executions strategy-data) u1)
              (get failed-executions strategy-data)
            ),
            total-ai-profit: (+ (get total-ai-profit strategy-data) value-change)
          })
        )
      )
      
      (map-set vault-risk-metrics
        { vault-id: vault-id }
        (merge risk-metrics {
          var-95: (/ (* volatility u164) u100),
          var-99: (/ (* volatility u233) u100),
          expected-shortfall: (/ (* volatility u200) u100),
          max-drawdown: drawdown-from-peak,
          volatility: volatility
        })
      )
      
      (let (
        (tvl-change (- (to-int new-value) (to-int old-value)))
      )
        (var-set total-value-locked (to-uint (+ (to-int (var-get total-value-locked)) tvl-change)))
        (var-set last-global-update current-height)
      )
      
      (let (
        (limits (unwrap! (get-vault-limits vault-id) (ok true)))
        (emergency-threshold (get emergency-exit-threshold limits))
      )
        (if (>= drawdown-from-peak emergency-threshold)
          (map-set vaults
            { vault-id: vault-id }
            (merge vault-data {
              emergency-pause: true
            })
          )
          (ok true)
        )
      )
      
      (ok new-value)
    )
  )
)

;; ============ EMERGENCY FUNCTIONS ============
(define-public (emergency-pause-vault (vault-id uint) (reason (string-ascii 128)))
  (let (
    (vault-data (unwrap! (get-vault-info vault-id) ERR-VAULT-NOT-FOUND))
  )
    (asserts! (or (is-eq tx-sender (get creator vault-data)) (is-eq tx-sender CONTRACT-OWNER)) ERR-UNAUTHORIZED)
    
    (map-set vaults
      { vault-id: vault-id }
      (merge vault-data {
        emergency-pause: true,
        last-update: block-height
      })
    )
    
    (print { event: "emergency-pause", vault-id: vault-id, reason: reason, timestamp: block-height })
    
    (ok true)
  )
)

(define-public (emergency-unpause-vault (vault-id uint))
  (let (
    (vault-data (unwrap! (get-vault-info vault-id) ERR-VAULT-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    
    (map-set vaults
      { vault-id: vault-id }
      (merge vault-data {
        emergency-pause: false,
        last-update: block-height
      })
    )
    
    (ok true)
  )
)

;; ============ ADMIN FUNCTIONS ============
(define-public (pause-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (var-set contract-paused true)
    (var-set last-global-update block-height)
    (ok true)
  )
)

(define-public (unpause-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (var-set contract-paused false)
    (var-set last-global-update block-height)
    (ok true)
  )
)

(define-public (update-performance-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (<= new-fee u2000) ERR-INVALID-AMOUNT)
    (var-set performance-fee new-fee)
    (ok true)
  )
)

(define-public (update-treasury-address (new-treasury principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (var-set treasury-address new-treasury)
    (ok true)
  )
)

(define-public (set-system-parameters (max-vaults-per-user-new uint) (global-multiplier uint) (risk-factor uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (<= max-vaults-per-user-new u100) ERR-INVALID-AMOUNT)
    (asserts! (<= global-multiplier u500) ERR-INVALID-AMOUNT)
    (asserts! (<= risk-factor u200) ERR-INVALID-AMOUNT)
    
    (var-set max-vaults-per-user max-vaults-per-user-new)
    (var-set global-performance-multiplier global-multiplier)
    (var-set risk-adjustment-factor risk-factor)
    (var-set last-global-update block-height)
    
    (ok true)
  )
)
