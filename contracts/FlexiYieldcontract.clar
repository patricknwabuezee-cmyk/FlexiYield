;; title: FlexiYieldcontract
;; version:
;; summary:
;; description:

;; title: FlexiYield - Adaptive Yield Farming Protocol
;; version: 1.0.0
;; summary: AI-driven yield farming with sentiment-based portfolio rebalancing
;; description: A decentralized yield farming protocol that uses sentiment analysis and market psychology
;;              to automatically rebalance portfolios and optimize yields based on market conditions

;; traits
(define-trait yield-strategy-trait
  (
    (get-apy () (response uint uint))
    (deposit (uint) (response uint uint))
    (withdraw (uint) (response uint uint))
    (get-total-deposits () (response uint uint))
  )
)

(define-trait sentiment-oracle-trait
  (
    (get-fear-greed-index () (response uint uint))
    (get-social-sentiment () (response int uint))
  )
)

;; token definitions
(define-fungible-token flexi-yield-token)

;; constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-NOT-AUTHORIZED (err u101))
(define-constant ERR-INSUFFICIENT-BALANCE (err u102))
(define-constant ERR-INVALID-AMOUNT (err u103))
(define-constant ERR-STRATEGY-NOT-FOUND (err u104))
(define-constant ERR-INVALID-RISK-LEVEL (err u105))
(define-constant ERR-REBALANCE-COOLDOWN (err u106))
(define-constant ERR-SENTIMENT-FETCH-FAILED (err u107))
(define-constant ERR-INVALID-THRESHOLD (err u108))

(define-constant PRECISION u1000000) ;; 6 decimal precision
(define-constant MAX-STRATEGIES u10)
(define-constant REBALANCE-COOLDOWN u144) ;; ~24 hours in blocks
(define-constant MIN-DEPOSIT u1000000) ;; Minimum 1 STX deposit

;; Risk tolerance levels
(define-constant RISK-CONSERVATIVE u1)
(define-constant RISK-MODERATE u2)
(define-constant RISK-AGGRESSIVE u3)

;; Sentiment thresholds
(define-constant FEAR-THRESHOLD u25) ;; Below 25 = Extreme Fear
(define-constant GREED-THRESHOLD u75) ;; Above 75 = Extreme Greed
(define-constant SENTIMENT-NEGATIVE -50)
(define-constant SENTIMENT-POSITIVE 50)

;; data vars
(define-data-var protocol-fee-rate uint u10000) ;; 1% = 10000 (out of 1000000)
(define-data-var total-value-locked uint u0)
(define-data-var active-strategies uint u0)
(define-data-var last-rebalance-block uint u0)
(define-data-var emergency-pause bool false)
(define-data-var sentiment-oracle principal tx-sender)
(define-data-var min-rebalance-threshold uint u50000) ;; 5% minimum change to trigger rebalance
;; Track total allocation weights across active strategies (must be <= 100)
(define-data-var total-allocation-weight uint u0)
;; Rebalance ID counter (for future use when recording history)
(define-data-var next-rebalance-id uint u0)

;; data maps
(define-map user-portfolios
  { user: principal }
  {
    total-deposited: uint,
    flexi-tokens: uint,
    risk-tolerance: uint,
    last-deposit-block: uint,
    auto-rebalance: bool
  }
)

(define-map yield-strategies
  { strategy-id: uint }
  {
    name: (string-ascii 50),
    contract-address: principal,
    current-apy: uint,
    total-allocated: uint,
    risk-level: uint,
    active: bool,
    weight: uint ;; allocation weight out of 100
  }
)

(define-map sentiment-triggers
  { trigger-id: uint }
  {
    condition-type: (string-ascii 20), ;; "fear-greed" or "social"
    threshold-value: int,
    target-strategy: uint,
    allocation-change: uint,
    active: bool
  }
)

(define-map user-sentiment-bias
  { user: principal }
  {
    fear-bias: int,
    greed-bias: int,
    social-influence: uint,
    correction-factor: uint
  }
)

(define-map rebalance-history
  { rebalance-id: uint }
  {
    block-height: uint,
    trigger-reason: (string-ascii 50),
    old-allocation: (list 10 uint),
    new-allocation: (list 10 uint),
    sentiment-score: int,
    fear-greed-index: uint
  }
)

;; public functions

;; Initialize a new yield strategy
(define-public (add-yield-strategy 
  (strategy-id uint)
  (name (string-ascii 50))
  (contract-address principal)
  (risk-level uint)
  (initial-weight uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (asserts! (<= risk-level u3) ERR-INVALID-RISK-LEVEL)
    (asserts! (<= initial-weight u100) ERR-INVALID-AMOUNT)
    (asserts! (< (var-get active-strategies) MAX-STRATEGIES) ERR-INVALID-AMOUNT)
    (asserts! (<= (+ (var-get total-allocation-weight) initial-weight) u100) ERR-INVALID-AMOUNT)
    
    (map-set yield-strategies
      { strategy-id: strategy-id }
      {
        name: name,
        contract-address: contract-address,
        current-apy: u0,
        total-allocated: u0,
        risk-level: risk-level,
        active: true,
        weight: initial-weight
      }
    )
    (var-set active-strategies (+ (var-get active-strategies) u1))
    (var-set total-allocation-weight (+ (var-get total-allocation-weight) initial-weight))
    (print { event: "strategy-added", strategy-id: strategy-id, weight: initial-weight })
    (ok true)
  )
)

;; User deposits STX and receives FlexiYield tokens
(define-public (deposit (amount uint) (risk-tolerance uint))
  (let
    (
      (user tx-sender)
      (current-portfolio (default-to 
        { total-deposited: u0, flexi-tokens: u0, risk-tolerance: u1, last-deposit-block: u0, auto-rebalance: true }
        (map-get? user-portfolios { user: user })
      ))
      (flexi-tokens-to-mint (calculate-flexi-tokens amount))
    )
    (asserts! (not (var-get emergency-pause)) ERR-NOT-AUTHORIZED)
    (asserts! (>= amount MIN-DEPOSIT) ERR-INVALID-AMOUNT)
    (asserts! (<= risk-tolerance u3) ERR-INVALID-RISK-LEVEL)
    
    ;; Transfer STX from user
    (try! (stx-transfer? amount user (as-contract tx-sender)))
    
    ;; Mint FlexiYield tokens
    (try! (ft-mint? flexi-yield-token flexi-tokens-to-mint user))
    
    ;; Update user portfolio
    (map-set user-portfolios
      { user: user }
      {
        total-deposited: (+ (get total-deposited current-portfolio) amount),
        flexi-tokens: (+ (get flexi-tokens current-portfolio) flexi-tokens-to-mint),
        risk-tolerance: risk-tolerance,
        ;; Using last-rebalance-block as a placeholder for current block height
        last-deposit-block: (var-get last-rebalance-block),
        auto-rebalance: (get auto-rebalance current-portfolio)
      }
    )
    
    ;; Update TVL
    (var-set total-value-locked (+ (var-get total-value-locked) amount))
    
    ;; Trigger allocation if conditions are met
    (allocate-to-strategies amount risk-tolerance)
    (print { event: "deposit", user: user, amount: amount, minted: flexi-tokens-to-mint })
    (ok flexi-tokens-to-mint)
  )
)

;; User withdraws by burning FlexiYield tokens
(define-public (withdraw (flexi-tokens uint))
  (let
    (
      (user tx-sender)
      (user-portfolio (unwrap! (map-get? user-portfolios { user: user }) ERR-NOT-AUTHORIZED))
      (stx-to-withdraw (calculate-stx-withdrawal flexi-tokens))
    )
    (asserts! (not (var-get emergency-pause)) ERR-NOT-AUTHORIZED)
    (asserts! (>= (get flexi-tokens user-portfolio) flexi-tokens) ERR-INSUFFICIENT-BALANCE)
    
    ;; Burn FlexiYield tokens
    (try! (ft-burn? flexi-yield-token flexi-tokens user))
    
    ;; Update user portfolio
    (map-set user-portfolios
      { user: user }
      (merge user-portfolio {
        total-deposited: (if (>= (get total-deposited user-portfolio) stx-to-withdraw)
          (- (get total-deposited user-portfolio) stx-to-withdraw)
          u0),
        flexi-tokens: (- (get flexi-tokens user-portfolio) flexi-tokens)
      })
    )
    
    ;; Withdraw from strategies
    (withdraw-from-strategies stx-to-withdraw)
    
    ;; Transfer STX to user
    (try! (as-contract (stx-transfer? stx-to-withdraw tx-sender user)))
    
    ;; Update TVL
    (var-set total-value-locked 
      (if (>= (var-get total-value-locked) stx-to-withdraw)
        (- (var-get total-value-locked) stx-to-withdraw)
        u0))
    (print { event: "withdraw", user: user, burned: flexi-tokens, amount: stx-to-withdraw })
    (ok stx-to-withdraw)
  )
)

;; Rebalance portfolios based on sentiment analysis
(define-public (rebalance-with-sentiment)
  (let
    (
      ;; Using last-rebalance-block + 1 as a placeholder for current block-height
      (current-block (+ (var-get last-rebalance-block) u1))
      (last-rebalance (var-get last-rebalance-block))
      (sentiment-data (fetch-sentiment-data))
      (fear-greed-index (get fear-greed-index sentiment-data))
      (social-sentiment (get social-sentiment sentiment-data))
    )
    (asserts! (not (var-get emergency-pause)) ERR-NOT-AUTHORIZED)
    (asserts! (>= (- current-block last-rebalance) REBALANCE-COOLDOWN) ERR-REBALANCE-COOLDOWN)
    
    ;; Determine rebalance strategy based on sentiment
    (let
      (
        (new-allocations (calculate-sentiment-based-allocation fear-greed-index social-sentiment))
        (rebalance-needed (is-rebalance-needed new-allocations))
      )
      (if rebalance-needed
        (begin
          (execute-rebalance new-allocations)
          (var-set last-rebalance-block current-block)
          (print { event: "rebalance", block: current-block, fear-greed: fear-greed-index })
          (ok true)
        )
        (ok false)
      )
    )
  )
)

;; Set the sentiment oracle principal (owner-only)
(define-public (set-sentiment-oracle (oracle principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (var-set sentiment-oracle oracle)
    (print { event: "oracle-updated", oracle: oracle })
    (ok true)
  )
)

;; Update yield strategy metadata: activate/deactivate, update weight and/or contract
;; new-weight / new-contract are optional; if none provided, current values remain
(define-public (update-yield-strategy
  (strategy-id uint)
  (active bool)
  (new-weight (optional uint))
  (new-contract (optional principal)))
  (let
    (
      (current (unwrap! (map-get? yield-strategies { strategy-id: strategy-id }) ERR-STRATEGY-NOT-FOUND))
      (weight-after (default-to (get weight current) new-weight))
      (contract-after (default-to (get contract-address current) new-contract))
      (was-active (get active current))
      (prev-weight (get weight current))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (asserts! (<= weight-after u100) ERR-INVALID-AMOUNT)
    ;; compute new total-allocation-weight
    (let
      (
        (total (var-get total-allocation-weight))
        (total-minus (if was-active (- total prev-weight) total))
        (total-plus (if active (+ total-minus weight-after) total-minus))
      )
      (asserts! (<= total-plus u100) ERR-INVALID-AMOUNT)
      (map-set yield-strategies
        { strategy-id: strategy-id }
        {
          name: (get name current),
          contract-address: contract-after,
          current-apy: (get current-apy current),
          total-allocated: (get total-allocated current),
          risk-level: (get risk-level current),
          active: active,
          weight: weight-after
        }
      )
      (var-set total-allocation-weight total-plus)
      (print { event: "strategy-updated", strategy-id: strategy-id, active: active, weight: weight-after })
      (ok true)
    )
  )
)

;; Set sentiment trigger conditions
(define-public (set-sentiment-trigger
  (trigger-id uint)
  (condition-type (string-ascii 20))
  (threshold-value int)
  (target-strategy uint)
  (allocation-change uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    
    (map-set sentiment-triggers
      { trigger-id: trigger-id }
      {
        condition-type: condition-type,
        threshold-value: threshold-value,
        target-strategy: target-strategy,
        allocation-change: allocation-change,
        active: true
      }
    )
    (ok true)
  )
)

;; Update user's emotional bias profile
(define-public (update-bias-profile (fear-bias int) (greed-bias int) (social-influence uint))
  (let
    ((user tx-sender))
    (map-set user-sentiment-bias
      { user: user }
      {
        fear-bias: fear-bias,
        greed-bias: greed-bias,
        social-influence: social-influence,
        correction-factor: (calculate-correction-factor fear-bias greed-bias social-influence)
      }
    )
    (ok true)
  )
)

;; Emergency pause/unpause
(define-public (set-emergency-pause (pause bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (var-set emergency-pause pause)
    (ok true)
  )
)

;; Update protocol fee
(define-public (set-protocol-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (asserts! (<= new-fee u50000) ERR-INVALID-AMOUNT) ;; Max 5%
    (var-set protocol-fee-rate new-fee)
    (ok true)
  )
)

;; read only functions

;; Get user portfolio information
(define-read-only (get-user-portfolio (user principal))
  (map-get? user-portfolios { user: user })
)

;; Get yield strategy information
(define-read-only (get-yield-strategy (strategy-id uint))
  (map-get? yield-strategies { strategy-id: strategy-id })
)

;; Calculate current portfolio value for user
(define-read-only (get-portfolio-value (user principal))
  (match (map-get? user-portfolios { user: user })
    portfolio (ok (calculate-portfolio-value (get flexi-tokens portfolio)))
    (err ERR-NOT-AUTHORIZED)
  )
)

;; Get total value locked
(define-read-only (get-tvl)
  (ok (var-get total-value-locked))
)

;; Get current protocol statistics
(define-read-only (get-protocol-stats)
  (ok {
    tvl: (var-get total-value-locked),
    active-strategies: (var-get active-strategies),
    protocol-fee: (var-get protocol-fee-rate),
    last-rebalance: (var-get last-rebalance-block),
    emergency-pause: (var-get emergency-pause)
  })
)

;; Get user's sentiment bias profile
(define-read-only (get-user-bias-profile (user principal))
  (map-get? user-sentiment-bias { user: user })
)

;; Calculate expected APY for user based on risk tolerance
(define-read-only (calculate-user-apy (user principal))
  (match (map-get? user-portfolios { user: user })
    portfolio (ok (calculate-weighted-apy (get risk-tolerance portfolio)))
    (err ERR-NOT-AUTHORIZED)
  )
)

;; private functions

;; Calculate FlexiYield tokens to mint based on STX deposit
(define-private (calculate-flexi-tokens (stx-amount uint))
  (let
    ((current-tvl (var-get total-value-locked))
     (total-supply (ft-get-supply flexi-yield-token)))
    (if (is-eq total-supply u0)
      (* stx-amount PRECISION) ;; Initial 1:1 ratio with precision
      (/ (* stx-amount total-supply) current-tvl)
    )
  )
)

;; Calculate STX to return when burning FlexiYield tokens
(define-private (calculate-stx-withdrawal (flexi-tokens uint))
  (let
    ((current-tvl (var-get total-value-locked))
     (total-supply (ft-get-supply flexi-yield-token)))
    (if (is-eq total-supply u0)
      u0
      (/ (* flexi-tokens current-tvl) total-supply)
    )
  )
)

;; Calculate portfolio value based on FlexiYield tokens
(define-private (calculate-portfolio-value (flexi-tokens uint))
  (let
    ((current-tvl (var-get total-value-locked))
     (total-supply (ft-get-supply flexi-yield-token)))
    (if (is-eq total-supply u0)
      u0
      (/ (* flexi-tokens current-tvl) total-supply)
    )
  )
)

;; Fetch sentiment data from oracle
(define-private (fetch-sentiment-data)
  (let
    ((oracle (var-get sentiment-oracle)))
    ;; This would integrate with actual sentiment oracle
    ;; For now, returning mock data
    {
      fear-greed-index: u50,
      social-sentiment: 0
    }
  )
)

;; Calculate new allocation weights based on sentiment
(define-private (calculate-sentiment-based-allocation (fear-greed uint) (social-sentiment int))
  (let
    ((base-conservative-weight u40)
     (base-moderate-weight u35)
     (base-aggressive-weight u25))
    
    ;; Adjust weights based on fear-greed index
    (let
      ((fear-adjustment (if (< fear-greed FEAR-THRESHOLD) u15 u0))
       (greed-adjustment (if (> fear-greed GREED-THRESHOLD) u15 u0)))
      
      ;; Return adjusted allocation
      (list
        (+ base-conservative-weight fear-adjustment (if (> greed-adjustment u0) u0 u10))
        base-moderate-weight
        (+ base-aggressive-weight greed-adjustment (if (> fear-adjustment u0) u0 u10))
      )
    )
  )
)

;; Check if rebalance is needed based on threshold
(define-private (is-rebalance-needed (new-allocations (list 3 uint)))
  (let
    ((threshold (var-get min-rebalance-threshold)))
    ;; Simplified check - in practice would compare with current allocations
    true
  )
)

;; Execute portfolio rebalance
(define-private (execute-rebalance (new-allocations (list 3 uint)))
  ;; This would implement the actual rebalancing logic
  ;; Moving funds between strategies based on new allocations
  true
)

;; Allocate funds to strategies based on amount and risk tolerance
(define-private (allocate-to-strategies (amount uint) (risk-tolerance uint))
  ;; Implementation would distribute funds across strategies
  ;; based on user's risk tolerance and current strategy weights
  true
)

;; Withdraw from strategies proportionally
(define-private (withdraw-from-strategies (amount uint))
  ;; Implementation would withdraw from strategies proportionally
  ;; to maintain desired allocation ratios
  true
)

;; Calculate weighted APY based on risk tolerance
(define-private (calculate-weighted-apy (risk-tolerance uint))
  (if (is-eq risk-tolerance u1)
    u80000
    (if (is-eq risk-tolerance u2)
      u120000
      (if (is-eq risk-tolerance u3)
        u180000
        u80000)))
)

;; Calculate correction factor for emotional bias
(define-private (calculate-correction-factor (fear-bias int) (greed-bias int) (social-influence uint))
  (let
    ((bias-magnitude (+ (if (> fear-bias 0) fear-bias (- 0 fear-bias))
                       (if (> greed-bias 0) greed-bias (- 0 greed-bias)))))
    ;; Higher bias = higher correction factor
    (+ u100 (/ (* (to-uint bias-magnitude) u50) u100))
  )
)