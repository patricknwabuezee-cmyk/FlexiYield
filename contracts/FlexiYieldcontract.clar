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
