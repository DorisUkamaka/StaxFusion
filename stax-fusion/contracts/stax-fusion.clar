;; StaxFusion: AI-Powered DeFi Yield Optimizer
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
(define-constant ERR_PROTOCOL_NOT_FOUND (err u108))

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

;; =============================================================================
;; PROTOCOL MANAGEMENT FUNCTIONS
;; =============================================================================

(define-public (register-protocol
        (name (string-ascii 32))
        (contract-address principal)
        (initial-apy uint)
        (risk-score uint)
    )
    (let (
            (new-protocol-id (+ (var-get protocol-count) u1))
            (current-block (get-current-block-height))
        )
        (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
        (asserts! (not (is-emergency-paused)) ERR_UNAUTHORIZED)
        (asserts! (and (>= risk-score u1) (<= risk-score u10))
            ERR_RISK_TOLERANCE_INVALID
        )
        (asserts! (<= new-protocol-id MAX_PROTOCOLS) ERR_PROTOCOL_NOT_ACTIVE)
        (map-set protocols { protocol-id: new-protocol-id } {
            name: name,
            contract-address: contract-address,
            current-apy: initial-apy,
            risk-score: risk-score,
            tvl: u0,
            is-active: true,
            last-updated: current-block,
        })
        (var-set protocol-count new-protocol-id)
        (ok new-protocol-id)
    )
)

(define-public (update-protocol-apy
        (protocol-id uint)
        (new-apy uint)
    )
    (let ((current-block (get-current-block-height)))
        (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
        (asserts! (not (is-emergency-paused)) ERR_UNAUTHORIZED)
        (match (map-get? protocols { protocol-id: protocol-id })
            protocol-data (begin
                (map-set protocols { protocol-id: protocol-id }
                    (merge protocol-data {
                        current-apy: new-apy,
                        last-updated: current-block,
                    })
                )
                (ok true)
            )
            ERR_PROTOCOL_NOT_FOUND
        )
    )
)

(define-public (toggle-protocol-status (protocol-id uint))
    (let ((current-block (get-current-block-height)))
        (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
        (match (map-get? protocols { protocol-id: protocol-id })
            protocol-data (begin
                (map-set protocols { protocol-id: protocol-id }
                    (merge protocol-data {
                        is-active: (not (get is-active protocol-data)),
                        last-updated: current-block,
                    })
                )
                (ok true)
            )
            ERR_PROTOCOL_NOT_FOUND
        )
    )
)

;; =============================================================================
;; AI STRATEGY ENGINE FUNCTIONS
;; =============================================================================

(define-private (calculate-risk-adjusted-yield
        (apy uint)
        (risk-score uint)
        (risk-tolerance uint)
    )
    (let ((risk-adjustment (if (<= risk-score risk-tolerance)
            u10000 ;; No penalty for acceptable risk
            (- u10000 (* (- risk-score risk-tolerance) u500)) ;; 5% penalty per risk level
        )))
        (/ (* apy risk-adjustment) u10000)
    )
)

(define-private (get-optimal-allocation
        (vault-id uint)
        (risk-tolerance uint)
    )
    (let (
            (protocol-1-data (unwrap-panic (map-get? protocols { protocol-id: u1 })))
            (protocol-2-data (unwrap-panic (map-get? protocols { protocol-id: u2 })))
            (protocol-3-data (unwrap-panic (map-get? protocols { protocol-id: u3 })))
        )
        ;; Simplified allocation logic - in production, this would be more sophisticated
        (if (<= risk-tolerance u3)
            ;; Conservative allocation
            (list
                {
                    protocol-id: u1,
                    allocation: u6000,
                }
                ;; 60% to safest
                {
                    protocol-id: u2,
                    allocation: u3000,
                }
                ;; 30% to medium
                {
                    protocol-id: u3,
                    allocation: u1000,
                }
                ;; 10% to higher risk
            )
            (if (<= risk-tolerance u7)
                ;; Moderate allocation
                (list
                    {
                        protocol-id: u1,
                        allocation: u4000,
                    }
                    ;; 40%
                    {
                        protocol-id: u2,
                        allocation: u4000,
                    }
                    ;; 40%
                    {
                        protocol-id: u3,
                        allocation: u2000,
                    }
                    ;; 20%
                )
                ;; Aggressive allocation
                (list
                    {
                        protocol-id: u1,
                        allocation: u2000,
                    }
                    ;; 20%
                    {
                        protocol-id: u2,
                        allocation: u3000,
                    }
                    ;; 30%
                    {
                        protocol-id: u3,
                        allocation: u5000,
                    }
                    ;; 50%
                )
            )
        )
    )
)

(define-private (calculate-rebalance-opportunity
        (vault-id uint)
        (current-allocations (list 10 {
            protocol-id: uint,
            allocation: uint,
        }))
    )
    (let (
            (vault-data (unwrap-panic (map-get? user-vaults { user: tx-sender })))
            (risk-tolerance (get risk-tolerance vault-data))
            (optimal-allocations (get-optimal-allocation vault-id risk-tolerance))
        )
        ;; Simplified calculation - returns true if rebalancing is beneficial
        (> (len current-allocations) u0)
        ;; Placeholder logic
    )
)

;; =============================================================================
;; YIELD CALCULATION FUNCTIONS
;; =============================================================================

(define-private (calculate-compound-yield
        (principal-amount uint)
        (apy uint)
        (time-periods uint)
    )
    (let (
            (rate-per-period (/ apy u365)) ;; Daily compounding
            (compound-factor (pow (+ u10000 rate-per-period) time-periods))
        )
        (/ (* principal-amount compound-factor) u10000)
    )
)

(define-private (estimate-vault-yield (vault-id uint))
    (let (
            (allocation-1 (default-to {
                allocated-amount: u0,
                allocation-percentage: u0,
                last-yield: u0,
                allocation-timestamp: u0,
            }
                (map-get? vault-allocations {
                    vault-id: vault-id,
                    protocol-id: u1,
                })
            ))
            (allocation-2 (default-to {
                allocated-amount: u0,
                allocation-percentage: u0,
                last-yield: u0,
                allocation-timestamp: u0,
            }
                (map-get? vault-allocations {
                    vault-id: vault-id,
                    protocol-id: u2,
                })
            ))
            (allocation-3 (default-to {
                allocated-amount: u0,
                allocation-percentage: u0,
                last-yield: u0,
                allocation-timestamp: u0,
            }
                (map-get? vault-allocations {
                    vault-id: vault-id,
                    protocol-id: u3,
                })
            ))
        )
        (+ (get last-yield allocation-1) (get last-yield allocation-2)
            (get last-yield allocation-3)
        )
    )
)

;; =============================================================================
;; DEPOSIT & WITHDRAWAL FUNCTIONS
;; =============================================================================
(define-private (allocate-to-protocol
        (allocation {
            protocol-id: uint,
            allocation: uint,
        })
        (context {
            vault-id: uint,
            amount: uint,
            block: uint,
        })
    )
    (let (
            (protocol-id (get protocol-id allocation))
            (allocation-pct (get allocation allocation))
            (vault-id (get vault-id context))
            (total-amount (get amount context))
            (current-block (get block context))
            (allocated-amount (/ (* total-amount allocation-pct) u10000))
        )
        (map-set vault-allocations {
            vault-id: vault-id,
            protocol-id: protocol-id,
        } {
            allocated-amount: allocated-amount,
            allocation-percentage: allocation-pct,
            last-yield: u0,
            allocation-timestamp: current-block,
        })
        context
        ;; Return context for fold continuation
    )
)

(define-private (auto-allocate-funds
        (vault-id uint)
        (risk-tolerance uint)
        (amount uint)
    )
    (let (
            (optimal-allocations (get-optimal-allocation vault-id risk-tolerance))
            (current-block (get-current-block-height))
            (allocation-result (fold allocate-to-protocol optimal-allocations {
                vault-id: vault-id,
                amount: amount,
                block: current-block,
            }))
        )
        ;; Allocate funds according to optimal strategy
        true
    )
)

(define-public (deposit (amount uint))
    (let (
            (user tx-sender)
            (current-block (get-current-block-height))
        )
        (asserts! (not (is-emergency-paused)) ERR_UNAUTHORIZED)
        (asserts! (>= amount MIN_DEPOSIT) ERR_INVALID_AMOUNT)
        (match (map-get? user-vaults { user: user })
            vault-data (let (
                    (new-balance (+ (get balance vault-data) amount))
                    (vault-id (get vault-id vault-data))
                )
                ;; Update user vault balance
                (map-set user-vaults { user: user }
                    (merge vault-data {
                        balance: new-balance,
                        last-deposit: current-block,
                    })
                )
                ;; Update total value locked
                (var-set total-value-locked
                    (+ (var-get total-value-locked) amount)
                )
                ;; Record deposit in history
                (map-set user-history {
                    user: user,
                    timestamp: current-block,
                } {
                    action: "DEPOSIT",
                    amount: amount,
                    vault-balance-after: new-balance,
                    protocols-involved: (list),
                })
                ;; Trigger automatic allocation
                (begin
                    (auto-allocate-funds vault-id (get risk-tolerance vault-data)
                        amount
                    )
                    (ok new-balance)
                )
            )
            ERR_VAULT_NOT_FOUND
        )
    )
)

(define-public (withdraw (amount uint))
    (let (
            (user tx-sender)
            (current-block (get-current-block-height))
        )
        (asserts! (not (is-emergency-paused)) ERR_UNAUTHORIZED)
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (match (map-get? user-vaults { user: user })
            vault-data (let (
                    (current-balance (get balance vault-data))
                    (vault-id (get vault-id vault-data))
                )
                (asserts! (>= current-balance amount) ERR_INSUFFICIENT_BALANCE)
                (let ((new-balance (- current-balance amount)))
                    ;; Update user vault balance
                    (map-set user-vaults { user: user }
                        (merge vault-data { balance: new-balance })
                    )
                    ;; Update total value locked
                    (var-set total-value-locked
                        (- (var-get total-value-locked) amount)
                    )
                    ;; Record withdrawal in history
                    (map-set user-history {
                        user: user,
                        timestamp: current-block,
                    } {
                        action: "WITHDRAWAL",
                        amount: amount,
                        vault-balance-after: new-balance,
                        protocols-involved: (list),
                    })
                    (ok new-balance)
                )
            )
            ERR_VAULT_NOT_FOUND
        )
    )
)

;; =============================================================================
;; AUTOMATED REBALANCING ENGINE
;; =============================================================================

(define-private (rebalance-to-protocol
        (allocation {
            protocol-id: uint,
            allocation: uint,
        })
        (context {
            vault-id: uint,
            amount: uint,
            block: uint,
        })
    )
    (let (
            (protocol-id (get protocol-id allocation))
            (allocation-pct (get allocation allocation))
            (vault-id (get vault-id context))
            (total-amount (get amount context))
            (current-block (get block context))
            (allocated-amount (/ (* total-amount allocation-pct) u10000))
        )
        ;; Set new allocation
        (map-set vault-allocations {
            vault-id: vault-id,
            protocol-id: protocol-id,
        } {
            allocated-amount: allocated-amount,
            allocation-percentage: allocation-pct,
            last-yield: u0,
            allocation-timestamp: current-block,
        })
        ;; Update protocol TVL
        (match (map-get? protocols { protocol-id: protocol-id })
            protocol-data
            (map-set protocols { protocol-id: protocol-id }
                (merge protocol-data { tvl: (+ (get tvl protocol-data) allocated-amount) })
            )
            false ;; Protocol not found
        )
        context
        ;; Return context for fold continuation
    )
)

(define-private (clear-vault-allocations (vault-id uint))
    (begin
        ;; Clear allocations for all protocols (simplified - in production would iterate through all)
        (map-delete vault-allocations {
            vault-id: vault-id,
            protocol-id: u1,
        })
        (map-delete vault-allocations {
            vault-id: vault-id,
            protocol-id: u2,
        })
        (map-delete vault-allocations {
            vault-id: vault-id,
            protocol-id: u3,
        })
        (ok true)
    )
)

(define-private (execute-rebalancing-strategy
        (vault-id uint)
        (risk-tolerance uint)
        (available-balance uint)
    )
    (let (
            (optimal-allocations (get-optimal-allocation vault-id risk-tolerance))
            (current-block (get-current-block-height))
        )
        ;; Clear existing allocations and check response
        (try! (clear-vault-allocations vault-id))
        ;; Apply new optimal allocations and check response
        (try! (fold rebalance-to-protocol optimal-allocations {
            vault-id: vault-id,
            amount: available-balance, ;; Fixed typo: available-balance
            block: current-block,
        }))
        (ok true)
    )
)

(define-public (trigger-rebalance)
    (let (
            (user tx-sender)
            (current-block (get-current-block-height))
        )
        (asserts! (not (is-emergency-paused)) ERR_UNAUTHORIZED)
        (match (map-get? user-vaults { user: user })
            vault-data (let (
                    (vault-id (get vault-id vault-data))
                    (last-rebalance (get last-rebalance vault-data))
                    (risk-tolerance (get risk-tolerance vault-data))
                    (vault-balance (get balance vault-data))
                )
                ;; Check cooldown period
                (asserts!
                    (>= (- current-block last-rebalance) REBALANCE_COOLDOWN)
                    ERR_REBALANCE_COOLDOWN
                )
                (asserts! (> vault-balance u0) ERR_INSUFFICIENT_BALANCE)
                ;; Calculate rebalancing fee
                (let (
                        (fee-amount (/ (* vault-balance (var-get rebalance-fee)) u10000))
                        (net-balance (- vault-balance fee-amount))
                    )
                    ;; Execute rebalancing strategy
                    (try! (execute-rebalancing-strategy vault-id risk-tolerance
                        net-balance
                    ))
                    ;; Update vault data
                    (map-set user-vaults { user: user }
                        (merge vault-data {
                            last-rebalance: current-block,
                            balance: net-balance,
                        })
                    )
                    ;; Record rebalancing in history
                    (map-set user-history {
                        user: user,
                        timestamp: current-block,
                    } {
                        action: "REBALANCE",
                        amount: fee-amount,
                        vault-balance-after: net-balance,
                        protocols-involved: (list u1 u2 u3),
                    })
                    (ok {
                        rebalanced: true,
                        fee-charged: fee-amount,
                        new-balance: net-balance,
                    })
                )
            )
            ERR_VAULT_NOT_FOUND
        )
    )
)

;; =============================================================================
;; YIELD HARVESTING & COMPOUNDING
;; =============================================================================

(define-public (harvest-yield)
    (let (
            (user tx-sender)
            (current-block (get-current-block-height))
        )
        (asserts! (not (is-emergency-paused)) ERR_UNAUTHORIZED)
        (match (map-get? user-vaults { user: user })
            vault-data (let (
                    (vault-id (get vault-id vault-data))
                    (current-balance (get balance vault-data))
                    (estimated-yield (estimate-vault-yield vault-id))
                )
                (if (> estimated-yield u0)
                    (let (
                            (new-balance (+ current-balance estimated-yield))
                            (new-total-earned (+ (get total-earned vault-data) estimated-yield))
                        )
                        ;; Update vault with harvested yield
                        (map-set user-vaults { user: user }
                            (merge vault-data {
                                balance: new-balance,
                                total-earned: new-total-earned,
                            })
                        )
                        ;; Update total value locked
                        (var-set total-value-locked
                            (+ (var-get total-value-locked) estimated-yield)
                        )
                        ;; Record yield harvest
                        (map-set user-history {
                            user: user,
                            timestamp: current-block,
                        } {
                            action: "YIELD_HARVEST",
                            amount: estimated-yield,
                            vault-balance-after: new-balance,
                            protocols-involved: (list u1 u2 u3),
                        })
                        (ok {
                            harvested: true,
                            yield-earned: estimated-yield,
                            new-balance: new-balance,
                        })
                    )
                    (ok {
                        harvested: false,
                        yield-earned: u0,
                        new-balance: current-balance,
                    })
                )
            )
            ERR_VAULT_NOT_FOUND
        )
    )
)

(define-public (compound-yield)
    (let ((user tx-sender))
        ;; Harvest yield first
        (let ((harvest-result (try! (harvest-yield))))
            ;; If yield was harvested, automatically reinvest
            (if (get harvested harvest-result)
                (let ((yield-amount (get yield-earned harvest-result)))
                    ;; Reinvest the harvested yield
                    (match (map-get? user-vaults { user: user })
                        vault-data (let (
                                (vault-id (get vault-id vault-data))
                                (risk-tolerance (get risk-tolerance vault-data))
                            )
                            (try! (auto-allocate-funds vault-id risk-tolerance
                                yield-amount
                            ))
                            (ok {
                                compounded: true,
                                amount: yield-amount,
                            })
                        )
                        ERR_VAULT_NOT_FOUND
                    )
                )
                (ok {
                    compounded: false,
                    amount: u0,
                })
            )
        )
    )
)

;; =============================================================================
;; EMERGENCY & ADMINISTRATIVE FUNCTIONS
;; =============================================================================

(define-public (emergency-pause)
    (begin
        (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
        (var-set emergency-pause true)
        (ok true)
    )
)

(define-public (emergency-unpause)
    (begin
        (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
        (var-set emergency-pause false)
        (ok true)
    )
)

(define-public (emergency-withdraw (user principal))
    (let ((current-block (get-current-block-height)))
        (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
        (asserts! (is-emergency-paused) ERR_UNAUTHORIZED)
        (match (map-get? user-vaults { user: user })
            vault-data (let ((vault-balance (get balance vault-data)))
                (if (> vault-balance u0)
                    (begin
                        ;; Zero out user balance
                        (map-set user-vaults { user: user }
                            (merge vault-data { balance: u0 })
                        )
                        ;; Update total value locked
                        (var-set total-value-locked
                            (- (var-get total-value-locked) vault-balance)
                        )
                        ;; Record emergency withdrawal
                        (map-set user-history {
                            user: user,
                            timestamp: current-block,
                        } {
                            action: "EMERGENCY_WITHDRAWAL",
                            amount: vault-balance,
                            vault-balance-after: u0,
                            protocols-involved: (list),
                        })
                        (ok vault-balance)
                    )
                    (ok u0)
                )
            )
            ERR_VAULT_NOT_FOUND
        )
    )
)

(define-public (update-rebalance-fee (new-fee uint))
    (begin
        (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
        (asserts! (<= new-fee u100) ERR_INVALID_AMOUNT) ;; Max 1% fee
        (var-set rebalance-fee new-fee)
        (ok true)
    )
)

(define-public (transfer-ownership (new-owner principal))
    (begin
        (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
        (var-set contract-owner new-owner)
        (ok true)
    )
)

;; =============================================================================
;; ANALYTICS & REPORTING FUNCTIONS
;; =============================================================================

(define-read-only (get-vault-performance (vault-id uint))
    (let (
            (allocation-1 (map-get? vault-allocations {
                vault-id: vault-id,
                protocol-id: u1,
            }))
            (allocation-2 (map-get? vault-allocations {
                vault-id: vault-id,
                protocol-id: u2,
            }))
            (allocation-3 (map-get? vault-allocations {
                vault-id: vault-id,
                protocol-id: u3,
            }))
        )
        {
            vault-id: vault-id,
            total-allocations: u3,
            allocation-1: allocation-1,
            allocation-2: allocation-2,
            allocation-3: allocation-3,
            estimated-apy: (estimate-vault-yield vault-id),
        }
    )
)

(define-read-only (get-protocol-rankings)
    (let (
            (protocol-1 (map-get? protocols { protocol-id: u1 }))
            (protocol-2 (map-get? protocols { protocol-id: u2 }))
            (protocol-3 (map-get? protocols { protocol-id: u3 }))
        )
        (list
            {
                protocol-id: u1,
                data: protocol-1,
                risk-adjusted-score: (match protocol-1
                    p1 (calculate-risk-adjusted-yield (get current-apy p1)
                        (get risk-score p1) u5
                    )
                    u0
                ),
            }
            {
                protocol-id: u2,
                data: protocol-2,
                risk-adjusted-score: (match protocol-2
                    p2 (calculate-risk-adjusted-yield (get current-apy p2)
                        (get risk-score p2) u5
                    )
                    u0
                ),
            }
            {
                protocol-id: u3,
                data: protocol-3,
                risk-adjusted-score: (match protocol-3
                    p3 (calculate-risk-adjusted-yield (get current-apy p3)
                        (get risk-score p3) u5
                    )
                    u0
                ),
            }
        )
    )
)

(define-read-only (get-user-history-summary (user principal))
    (let (
            (current-block (get-current-block-height))
            ;; Get recent history entries (simplified - would need pagination in production)
            (recent-entry-1 (map-get? user-history {
                user: user,
                timestamp: (- current-block u1),
            }))
            (recent-entry-2 (map-get? user-history {
                user: user,
                timestamp: (- current-block u2),
            }))
            (recent-entry-3 (map-get? user-history {
                user: user,
                timestamp: (- current-block u3),
            }))
        )
        {
            user: user,
            recent-actions: (list recent-entry-1 recent-entry-2 recent-entry-3),
            current-block: current-block,
        }
    )
)

;; =============================================================================
;; ADVANCED YIELD OPTIMIZATION
;; =============================================================================

(define-private (calculate-optimal-compound-frequency
        (vault-balance uint)
        (estimated-apy uint)
    )
    ;; Calculate optimal compounding frequency based on gas costs vs yield gains
    (if (< vault-balance u100000000) ;; Less than 100 STX
        u7 ;; Weekly compounding
        (if (< vault-balance u1000000000) ;; Less than 1000 STX
            u3 ;; Every 3 days
            u1 ;; Daily compounding for large amounts
        )
    )
)

(define-public (auto-compound-check)
    (let (
            (user tx-sender)
            (current-block (get-current-block-height))
        )
        (match (map-get? user-vaults { user: user })
            vault-data (let (
                    (vault-balance (get balance vault-data))
                    (last-rebalance (get last-rebalance vault-data))
                    (estimated-yield (estimate-vault-yield (get vault-id vault-data)))
                    (optimal-frequency (calculate-optimal-compound-frequency vault-balance
                        estimated-yield
                    ))
                )
                ;; Check if enough time has passed for optimal compounding
                (if (>= (- current-block last-rebalance)
                        (* optimal-frequency u144)
                    )
                    (compound-yield)
                    (ok {
                        should-compound: false,
                        blocks-until-optimal: (- (* optimal-frequency u144)
                            (- current-block last-rebalance)
                        ),
                    })
                )
            )
            ERR_VAULT_NOT_FOUND
        )
    )
)

;; =============================================================================
;; GOVERNANCE & STRATEGY UPDATES
;; =============================================================================

(define-public (update-strategy-parameters
        (risk-level uint)
        (max-allocation uint)
        (rebalance-threshold uint)
        (preferred-protocols (list 5 uint))
    )
    (begin
        (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
        (asserts! (and (>= risk-level u1) (<= risk-level u10))
            ERR_RISK_TOLERANCE_INVALID
        )
        (asserts! (<= max-allocation u10000) ERR_INVALID_AMOUNT)
        (map-set strategy-parameters { risk-level: risk-level } {
            max-single-protocol-allocation: max-allocation,
            rebalance-threshold: rebalance-threshold,
            preferred-protocols: preferred-protocols,
            risk-weights: (list u1000 u2000 u3000 u2500 u1500 u1000 u500 u300 u200 u100),
        })
        (ok true)
    )
)

;; =============================================================================
;; INTEGRATION HELPERS
;; =============================================================================

(define-read-only (simulate-deposit
        (user principal)
        (amount uint)
    )
    (match (map-get? user-vaults { user: user })
        vault-data (let (
                (current-balance (get balance vault-data))
                (new-balance (+ current-balance amount))
                (vault-id (get vault-id vault-data))
                (risk-tolerance (get risk-tolerance vault-data))
                (optimal-allocations (get-optimal-allocation vault-id risk-tolerance))
            )
            {
                current-balance: current-balance,
                new-balance: new-balance,
                optimal-allocations: optimal-allocations,
                estimated-apy: (estimate-vault-yield vault-id),
            }
        )
        {
            current-balance: u0,
            new-balance: amount,
            optimal-allocations: (list),
            estimated-apy: u0,
        }
    )
)

(define-read-only (get-rebalance-recommendation (user principal))
    (match (map-get? user-vaults { user: user })
        vault-data (let (
                (vault-id (get vault-id vault-data))
                (risk-tolerance (get risk-tolerance vault-data))
                (current-block (get-current-block-height))
                (last-rebalance (get last-rebalance vault-data))
                (blocks-since-rebalance (- current-block last-rebalance))
            )
            {
                should-rebalance: (>= blocks-since-rebalance REBALANCE_COOLDOWN),
                blocks-until-eligible: (if (>= blocks-since-rebalance REBALANCE_COOLDOWN)
                    u0
                    (- REBALANCE_COOLDOWN blocks-since-rebalance)
                ),
                optimal-allocations: (get-optimal-allocation vault-id risk-tolerance),
                estimated-improvement: u100, ;; Placeholder - would calculate actual improvement
            }
        )
        {
            should-rebalance: false,
            blocks-until-eligible: u0,
            optimal-allocations: (list),
            estimated-improvement: u0,
        }
    )
)
