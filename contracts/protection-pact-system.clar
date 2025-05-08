;; Protection Pact System
;;
;; This Clarity smart contract implements a decentralized protection system enabling users to:
;; 1. Contribute resources to a collective security vault.
;; 2. Acquire personalized protection certificates with adjustable parameters.
;; 3. Request asset recovery based on predetermined qualification criteria.
;; ================================
;; SYSTEM ADMINISTRATION
;; ================================

;; System administrator with privileged operations access
(define-constant protocol-admin tx-sender)

;; ================================
;; ERROR DEFINITIONS  
;; ================================
;; Comprehensive error codes for validation and execution failures

(define-constant error-unauthorized-access (err u100))
(define-constant error-insufficient-resources (err u101)) 
(define-constant error-transaction-rejected (err u102))
(define-constant error-invalid-parameter (err u103))
(define-constant error-certificate-pricing-invalid (err u104))
(define-constant error-vault-capacity-exceeded (err u105))
(define-constant error-certificate-unavailable (err u106))
(define-constant error-rate-validation-failed (err u107))
(define-constant error-asset-recovery-failed (err u108))

;; ================================
;; CONFIGURATION PARAMETERS
;; ================================
;; Core system parameters and operational thresholds

;; Default protection rate percentage (5.00%)
(define-data-var protection-rate uint u500)

;; Maximum capacity limit for collective security vault
(define-data-var vault-capacity-limit uint u1000000)

;; Current balance of collective security vault
(define-data-var vault-balance uint u0)

;; Maximum contribution allowed per participant
(define-data-var max-contribution-per-participant uint u10000)

;; ================================
;; PARTICIPANT DATA STORAGE
;; ================================
;; Data structures for tracking participant assets and certificates

;; Tracks participant contributions to the collective vault (in STX)
(define-map participant-contribution-ledger principal uint)

;; Tracks participant certificate assets (in STX)
(define-map participant-certificate-ledger principal uint)

;; Comprehensive certificate details for each participant
(define-map protection-certificates
  {participant: principal}
  {coverage: uint, rate: uint, status: bool})

;; ================================
;; CALCULATION UTILITIES
;; ================================

;; Calculates recovery amount based on coverage and protection rate
(define-private (calculate-recovery-amount (coverage uint))
  (/ (* coverage (var-get protection-rate)) u100))

;; Updates vault balance while enforcing system constraints
(define-private (update-vault-balance (delta int))
  (let (
    (current-balance (var-get vault-balance))
    (new-balance (if (< delta 0)
                   (if (>= current-balance (to-uint (- 0 delta)))
                       (- current-balance (to-uint (- 0 delta)))
                       u0)
                   (+ current-balance (to-uint delta))))
  )
    (asserts! (<= new-balance (var-get vault-capacity-limit)) error-vault-capacity-exceeded)
    (var-set vault-balance new-balance)
    (ok true)))

;; ================================
;; PARTICIPANT OPERATIONS
;; ================================

;; Contribute resources to the collective security vault
(define-public (contribute-to-vault (amount uint))
  (let (
    (current-contribution (default-to u0 (map-get? participant-contribution-ledger tx-sender)))
    (new-contribution (+ current-contribution amount))
  )
    (asserts! (<= new-contribution (var-get max-contribution-per-participant)) error-vault-capacity-exceeded)
    (map-set participant-contribution-ledger tx-sender new-contribution)
    (try! (update-vault-balance (to-int amount)))
    (ok true)))

;; Acquire a new protection certificate
(define-public (acquire-certificate (coverage-amount uint) (custom-rate uint))
  (let (
    (contribution-balance (default-to u0 (map-get? participant-contribution-ledger tx-sender)))
    (new-certificate-balance (+ (default-to u0 (map-get? participant-certificate-ledger tx-sender)) coverage-amount))
  )
    (asserts! (> coverage-amount u0) error-invalid-parameter)
    (asserts! (>= contribution-balance coverage-amount) error-insufficient-resources)
    (asserts! (<= custom-rate (var-get protection-rate)) error-rate-validation-failed)

    ;; Reallocate resources from contribution to certificate
    (map-set participant-contribution-ledger tx-sender (- contribution-balance coverage-amount))
    (map-set participant-certificate-ledger tx-sender new-certificate-balance)

    ;; Register certificate details
    (map-set protection-certificates {participant: tx-sender} 
             {coverage: coverage-amount, rate: custom-rate, status: true})

    (ok true)))
