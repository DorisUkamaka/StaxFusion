;; StaxFusion: AI-Powered DeFi Yield Optimizer
;; Commit 1: Core Data Structures & User Management
;; Author: StaxFusion Team
;; Description: Foundation contract with user management, vault structure, and basic constants

;; =============================================================================
;; CONSTANTS & ERROR CODES
;; =============================================================================

(define-constant CONTRACT_OWNER tx-sender)
(define-constant STACKS_BLOCK_TIME u144) ;; ~10 minutes in seconds

;; Error codes
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INSUFFICIENT_BALANCE (err u101))
(define-constant ERR_INVALID_AMOUNT (err u102))
(define-constant ERR_VAULT_NOT_FOUND (err u103))
(define-constant ERR_PROTOCOL_NOT_ACTIVE (err u104))
(define-constant ERR_RISK_TOLERANCE_INVALID (err u105))
(define-constant ERR_REBALANCE_COOLDOWN (err u106))
(define-constant ERR_SLIPPAGE_EXCEEDED (err u107))

;; Protocol constants
(define-constant MAX_PROTOCOLS u10)
(define-constant MIN_DEPOSIT u1000000) ;; 1 STX minimum
(define-constant REBALANCE_COOLDOWN u1008) ;; ~1 week in blocks
(define-constant MAX_SLIPPAGE u500) ;; 5% max slippage

;; =============================================================================
;; DATA VARIABLES
;; =============================================================================

(define-data-var contract-owner principal CONTRACT_OWNER)
(define-data-var total-value-locked uint u0)
(define-data-var protocol-count uint u0)
(define-data-var vault-count uint u0)
(define-data-var rebalance-fee uint u25) ;; 0.25% fee
(define-data-var emergency-pause bool false)

;; =============================================================================
;; DATA MAPS
;; =============================================================================

;; User vault information
(define-map user-vaults
    { user: principal }
    {
        balance: uint,
        risk-tolerance: uint, ;; 1-10 scale (1=conservative, 10=aggressive)
        last-deposit: uint,
        last-rebalance: uint,
        total-earned: uint,
        vault-id: uint,
    }
)

;; Protocol registry for yield farming targets
(define-map protocols
    { protocol-id: uint }
    {
        name: (string-ascii 32),
        contract-address: principal,
        current-apy: uint, ;; in basis points (100 = 1%)
        risk-score: uint, ;; 1-10 scale
        tvl: uint,
        is-active: bool,
        last-updated: uint,
    }
)

;; Vault allocation tracking
(define-map vault-allocations
    {
        vault-id: uint,
        protocol-id: uint,
    }
    {
        allocated-amount: uint,
        allocation-percentage: uint, ;; in basis points
        last-yield: uint,
        allocation-timestamp: uint,
    }
)

;; User position history for analytics
(define-map user-history
    {
        user: principal,
        timestamp: uint,
    }
    {
        action: (string-ascii 20),
        amount: uint,
        vault-balance-after: uint,
        protocols-involved: (list 10 uint),
    }
)

;; AI strategy parameters
(define-map strategy-parameters
    { risk-level: uint }
    {
        max-single-protocol-allocation: uint, ;; in basis points
        rebalance-threshold: uint, ;; minimum yield difference to trigger rebalance
        preferred-protocols: (list 5 uint),
        risk-weights: (list 10 uint),
    }
)

;; =============================================================================
;; PRIVATE FUNCTIONS
;; =============================================================================

(define-private (is-contract-owner)
    (is-eq tx-sender (var-get contract-owner))
)

(define-private (is-emergency-paused)
    (var-get emergency-pause)
)

(define-private (calculate-vault-id (user principal))
    (+ (var-get vault-count) u1)
)

(define-private (validate-risk-tolerance (risk uint))
    (and (>= risk u1) (<= risk u10))
)

(define-private (get-current-block-height)
    stacks-block-height
)

;; =============================================================================
;; READ-ONLY FUNCTIONS
;; =============================================================================

(define-read-only (get-user-vault (user principal))
    (map-get? user-vaults { user: user })
)

(define-read-only (get-protocol-info (protocol-id uint))
    (map-get? protocols { protocol-id: protocol-id })
)

(define-read-only (get-vault-allocation
        (vault-id uint)
        (protocol-id uint)
    )
    (map-get? vault-allocations {
        vault-id: vault-id,
        protocol-id: protocol-id,
    })
)

(define-read-only (get-total-value-locked)
    (var-get total-value-locked)
)

(define-read-only (get-contract-stats)
    {
        total-tvl: (var-get total-value-locked),
        active-protocols: (var-get protocol-count),
        total-vaults: (var-get vault-count),
        rebalance-fee: (var-get rebalance-fee),
        emergency-pause: (var-get emergency-pause),
    }
)

(define-read-only (get-user-stats (user principal))
    (match (map-get? user-vaults { user: user })
        vault-data (some {
            balance: (get balance vault-data),
            risk-tolerance: (get risk-tolerance vault-data),
            total-earned: (get total-earned vault-data),
            last-rebalance: (get last-rebalance vault-data),
            vault-id: (get vault-id vault-data),
        })
        none
    )
)

;; =============================================================================
;; PUBLIC FUNCTIONS - USER MANAGEMENT
;; =============================================================================

(define-public (create-vault (risk-tolerance uint))
    (let (
            (user tx-sender)
            (new-vault-id (calculate-vault-id user))
            (current-block (get-current-block-height))
        )
        (asserts! (not (is-emergency-paused)) ERR_UNAUTHORIZED)
        (asserts! (validate-risk-tolerance risk-tolerance)
            ERR_RISK_TOLERANCE_INVALID
        )
        (asserts! (is-none (map-get? user-vaults { user: user }))
            ERR_UNAUTHORIZED
        )
        (map-set user-vaults { user: user } {
            balance: u0,
            risk-tolerance: risk-tolerance,
            last-deposit: current-block,
            last-rebalance: current-block,
            total-earned: u0,
            vault-id: new-vault-id,
        })
        (var-set vault-count new-vault-id)
        ;; Record vault creation in history
        (map-set user-history {
            user: user,
            timestamp: current-block,
        } {
            action: "VAULT_CREATED",
            amount: u0,
            vault-balance-after: u0,
            protocols-involved: (list),
        })
        (ok new-vault-id)
    )
)

(define-public (update-risk-tolerance (new-risk-tolerance uint))
    (let (
            (user tx-sender)
            (current-block (get-current-block-height))
        )
        (asserts! (not (is-emergency-paused)) ERR_UNAUTHORIZED)
        (asserts! (validate-risk-tolerance new-risk-tolerance)
            ERR_RISK_TOLERANCE_INVALID
        )
        (match (map-get? user-vaults { user: user })
            vault-data (begin
                (map-set user-vaults { user: user }
                    (merge vault-data { risk-tolerance: new-risk-tolerance })
                )
                ;; Record risk tolerance update
                (map-set user-history {
                    user: user,
                    timestamp: current-block,
                } {
                    action: "RISK_UPDATED",
                    amount: new-risk-tolerance,
                    vault-balance-after: (get balance vault-data),
                    protocols-involved: (list),
                })
                (ok true)
            )
            ERR_VAULT_NOT_FOUND
        )
    )
)
