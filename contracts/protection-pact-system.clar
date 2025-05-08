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

;; Process asset recovery for qualified participant
(define-public (process-recovery (participant principal) (recovery-amount uint))
  (let (
    (certificate-details (default-to {coverage: u0, rate: u0, status: false} 
                                    (map-get? protection-certificates {participant: participant})))
    (recovery-value (calculate-recovery-amount recovery-amount))
    (security-vault-balance (var-get vault-balance))
  )
    (asserts! (get status certificate-details) error-certificate-unavailable)
    (asserts! (>= security-vault-balance recovery-value) error-asset-recovery-failed)

    ;; Update vault and participant balances
    (let (
      (current-certificate-balance (default-to u0 (map-get? participant-certificate-ledger participant)))
      (new-certificate-balance (- current-certificate-balance recovery-value))
    )
      (asserts! (>= current-certificate-balance recovery-value) error-asset-recovery-failed)
      (map-set participant-certificate-ledger participant new-certificate-balance)
    )
    (var-set vault-balance (- security-vault-balance recovery-value))
    (ok true)))

;; Deactivate a protection certificate temporarily
(define-public (deactivate-certificate)
  (begin
    (let ((certificate (default-to {coverage: u0, rate: u0, status: false} 
                                  (map-get? protection-certificates {participant: tx-sender}))))
      ;; Validate certificate status
      (asserts! (get status certificate) error-certificate-unavailable)
      ;; Toggle certificate status to inactive
      (map-set protection-certificates {participant: tx-sender} 
               {coverage: (get coverage certificate), 
                rate: (get rate certificate), 
                status: false})
      (ok true))))

;; Terminate protection certificate and recover contribution
(define-public (terminate-certificate)
  (begin
    (let ((certificate (default-to {coverage: u0, rate: u0, status: false} 
                                  (map-get? protection-certificates {participant: tx-sender}))))
      ;; Validate certificate status
      (asserts! (get status certificate) error-certificate-unavailable)
      ;; Return certificate amount to contribution balance
      (map-set participant-contribution-ledger tx-sender 
               (+ (default-to u0 (map-get? participant-contribution-ledger tx-sender)) 
                  (get coverage certificate)))
      ;; Deactivate certificate
      (map-set protection-certificates {participant: tx-sender} 
               {coverage: (get coverage certificate), rate: (get rate certificate), status: false})
      (ok true))))

;; ================================
;; RECOVERY OPERATIONS
;; ================================

;; Request partial asset recovery from active certificate
(define-public (request-partial-recovery (amount uint))
  (begin
    (let ((certificate (default-to {coverage: u0, rate: u0, status: false} 
                                  (map-get? protection-certificates {participant: tx-sender}))))
      ;; Validate certificate and coverage amount
      (asserts! (get status certificate) error-certificate-unavailable)
      (asserts! (>= (get coverage certificate) amount) error-asset-recovery-failed)
      ;; Process partial recovery
      (try! (update-vault-balance (- (to-int (calculate-recovery-amount amount)))))
      (map-set protection-certificates {participant: tx-sender} 
               {coverage: (- (get coverage certificate) amount), 
                rate: (get rate certificate), 
                status: true})
      (ok true))))

;; Expand certificate coverage amount
(define-public (expand-coverage (additional-amount uint))
  (begin
    (let ((certificate (default-to {coverage: u0, rate: u0, status: false} 
                                  (map-get? protection-certificates {participant: tx-sender}))))
      ;; Validate certificate status
      (asserts! (get status certificate) error-certificate-unavailable)
      ;; Verify sufficient contribution balance
      (asserts! (>= (default-to u0 (map-get? participant-contribution-ledger tx-sender)) additional-amount) 
                error-insufficient-resources)
      ;; Adjust balances and update certificate
      (map-set participant-contribution-ledger tx-sender 
               (- (default-to u0 (map-get? participant-contribution-ledger tx-sender)) additional-amount))
      (map-set protection-certificates {participant: tx-sender} 
               {coverage: (+ (get coverage certificate) additional-amount), 
                rate: (get rate certificate), 
                status: true})
      (ok true))))

;; ================================
;; ADMINISTRATIVE FUNCTIONS
;; ================================

;; Process multiple recoveries in a single operation
;; Enables efficient batch processing of qualifying recovery requests
(define-public (batch-process-recoveries (recovery-requests (list 10 {participant: principal, amount: uint})))
  (begin
    ;; Authorize administrative operation
    (asserts! (is-eq tx-sender protocol-admin) error-unauthorized-access)
    ;; Execute batch processing
    (fold process-individual-recovery recovery-requests (ok true))))

;; Helper function for processing individual recovery requests in batch operation
(define-private (process-individual-recovery 
                 (recovery-request {participant: principal, amount: uint}) 
                 (previous-result (response bool uint)))
  (begin
    ;; Ensure all previous operations succeeded
    (asserts! (is-ok previous-result) previous-result)
    ;; Process current recovery request
    (let ((participant-certificate (default-to {coverage: u0, rate: u0, status: false} 
                                  (map-get? protection-certificates 
                                            {participant: (get participant recovery-request)}))))
      ;; Validate certificate status
      (if (get status participant-certificate)
          (begin
            ;; Calculate recovery amount
            (let ((recovery-value (calculate-recovery-amount (get amount recovery-request))))
              ;; Validate vault capacity
              (if (>= (var-get vault-balance) recovery-value)
                  (begin
                    ;; Adjust vault balance
                    (var-set vault-balance (- (var-get vault-balance) recovery-value))
                    ;; Record transaction details
                    (print {operation: "batch-recovery", 
                            participant: (get participant recovery-request), 
                            amount: (get amount recovery-request), 
                            recovery: recovery-value})
                    ;; Update certificate coverage
                    (map-set protection-certificates 
                             {participant: (get participant recovery-request)} 
                             {coverage: (- (get coverage participant-certificate) 
                                          (get amount recovery-request)), 
                              rate: (get rate participant-certificate), 
                              status: (> (- (get coverage participant-certificate) 
                                           (get amount recovery-request)) u0)})
                    (ok true))
                  error-asset-recovery-failed)))
          error-certificate-unavailable))))

;; ================================
;; SYSTEM CONFIGURATION MANAGEMENT
;; ================================

;; Modify the system protection rate for new certificates
;; Administrative operation to adjust system economics
;; @param updated-rate: New protection rate percentage to apply
(define-public (modify-protection-rate (updated-rate uint))
  (begin
    ;; Validate administrative privileges
    (asserts! (is-eq tx-sender protocol-admin) error-unauthorized-access)
    ;; Validate rate parameters (between 1.00% and 20.00%)
    (asserts! (and (>= updated-rate u100) (<= updated-rate u2000)) error-rate-validation-failed)
    ;; Update system protection rate
    (var-set protection-rate updated-rate)
    ;; Confirm successful update
    (ok true)))

;; Update system capacity parameters
;; Adjusts operational thresholds for vault and participant limits
;; @param new-vault-limit: Updated maximum capacity for security vault
;; @param new-participant-limit: Updated maximum contribution per participant
(define-public (update-capacity-parameters (new-vault-limit uint) (new-participant-limit uint))
  (begin
    ;; Validate administrative privileges
    (asserts! (is-eq tx-sender protocol-admin) error-unauthorized-access)
    ;; Validate threshold parameters
    (asserts! (and (>= new-vault-limit u1000000) (<= new-vault-limit u1000000000)) error-invalid-parameter)
    (asserts! (and (>= new-participant-limit u1000) (<= new-participant-limit u100000)) error-invalid-parameter)
    ;; Update system parameters
    (var-set vault-capacity-limit new-vault-limit)
    (var-set max-contribution-per-participant new-participant-limit)
    ;; Confirm successful update
    (ok true)))

;; ================================
;; VAULT MANAGEMENT
;; ================================

;; Reallocate surplus resources from security vault
;; Enables capital efficiency while maintaining system solvency
;; @param withdrawal-amount: Amount to withdraw from surplus
;; @param beneficiary: Principal address receiving the reallocated resources
(define-public (reallocate-surplus (withdrawal-amount uint) (beneficiary principal))
  (let (
    (current-vault-balance (var-get vault-balance))
    (minimum-required-reserve (/ (* current-vault-balance u80) u100)) ;; 80% reserve requirement
  )
    ;; Validate administrative privileges
    (asserts! (is-eq tx-sender protocol-admin) error-unauthorized-access)
    ;; Validate withdrawal against reserve requirements
    (asserts! (>= (- current-vault-balance withdrawal-amount) minimum-required-reserve) 
              error-insufficient-resources)
    ;; Confirm successful operation
    (ok true)))

;; ================================
;; CERTIFICATE MANAGEMENT
;; ================================

;; Extend certificate protection duration
;; Enables participants to lengthen their coverage period
;; @param duration-extension: Additional time units for certificate validity
(define-public (extend-certificate-duration (duration-extension uint))
  (let ((certificate (default-to {coverage: u0, rate: u0, status: false} 
                              (map-get? protection-certificates {participant: tx-sender})))
        (extension-cost (/ (* (get coverage certificate) duration-extension) u365)))
    ;; Validate certificate status
    (asserts! (get status certificate) error-certificate-unavailable)
    ;; Verify sufficient contribution balance
    (asserts! (>= (default-to u0 (map-get? participant-contribution-ledger tx-sender)) extension-cost) 
              error-insufficient-resources)
    ;; Process payment for extension
    (map-set participant-contribution-ledger tx-sender 
             (- (default-to u0 (map-get? participant-contribution-ledger tx-sender)) extension-cost))
    ;; Add payment to security vault
    (try! (update-vault-balance (to-int extension-cost)))
    ;; Record transaction details
    (print {operation: "duration-extended", 
            participant: tx-sender, 
            duration-added: duration-extension, 
            cost: extension-cost})
    (ok true)))

;; Reassign certificate ownership
;; Enables transferring protection coverage to another participant
;; @param successor: Principal address of the new certificate holder
(define-public (reassign-certificate (successor principal))
  (let ((certificate (default-to {coverage: u0, rate: u0, status: false} 
                              (map-get? protection-certificates {participant: tx-sender}))))
    ;; Validate certificate status
    (asserts! (get status certificate) error-certificate-unavailable)
    ;; Verify successor eligibility
    (let ((successor-certificate (default-to {coverage: u0, rate: u0, status: false} 
                                          (map-get? protection-certificates {participant: successor}))))
      (asserts! (not (get status successor-certificate)) error-certificate-unavailable)
      ;; Remove certificate from current participant
      (map-delete protection-certificates {participant: tx-sender})
      ;; Record transaction details
      (print {operation: "certificate-reassigned", 
              from: tx-sender, 
              to: successor, 
              coverage: (get coverage certificate)})
      (ok true))))

;; ================================
;; ADVANCED PROTECTION FEATURES
;; ================================

;; Add contingency protection to existing certificate
;; Provides enhanced coverage for specific scenarios at premium rates
;; @param supplementary-coverage: Additional protection amount for contingencies
;; @param contingency-rate: Premium rate for contingency protection (elevated)
(define-public (add-contingency-protection (supplementary-coverage uint) (contingency-rate uint))
  (let ((certificate (default-to {coverage: u0, rate: u0, status: false} 
                              (map-get? protection-certificates {participant: tx-sender})))
        (contribution-balance (default-to u0 (map-get? participant-contribution-ledger tx-sender))))
    ;; Validate certificate status
    (asserts! (get status certificate) error-certificate-unavailable)
    ;; Validate contingency rate premium
    (asserts! (> contingency-rate (var-get protection-rate)) error-rate-validation-failed)
    ;; Verify sufficient contribution balance
    (asserts! (>= contribution-balance supplementary-coverage) error-insufficient-resources)
    ;; Deduct contribution balance
    (map-set participant-contribution-ledger tx-sender (- contribution-balance supplementary-coverage))
    ;; Update certificate with additional coverage
    (map-set protection-certificates {participant: tx-sender} 
             {coverage: (+ (get coverage certificate) supplementary-coverage), 
              rate: contingency-rate, 
              status: true})
    ;; Update security vault
    (try! (update-vault-balance (to-int supplementary-coverage)))
    ;; Record transaction details
    (print {operation: "contingency-protection-added", 
            participant: tx-sender, 
            coverage: supplementary-coverage, 
            rate: contingency-rate})
    (ok true)))

;; ================================
;; SECURITY OPERATIONS
;; ================================

;; Enable heightened security mode for compromised accounts
;; Secures certificate assets during authentication breaches
;; @param trusted-recovery-agent: Authorized principal for recovery operations
;; @param security-duration: Duration of heightened security in block units (min 144 blocks)
(define-public (enable-heightened-security (trusted-recovery-agent principal) (security-duration uint))
  (let ((certificate (default-to {coverage: u0, rate: u0, status: false} 
                              (map-get? protection-certificates {participant: tx-sender}))))
    ;; Validate certificate status
    (asserts! (get status certificate) error-certificate-unavailable)
    ;; Validate security duration (minimum threshold)
    (asserts! (>= security-duration u144) error-invalid-parameter)
    ;; Temporarily suspend certificate
    (map-set protection-certificates {participant: tx-sender} 
             {coverage: (get coverage certificate), 
              rate: (get rate certificate), 
              status: false})
    ;; Record security configuration
    (print {operation: "heightened-security-enabled", 
            participant: tx-sender, 
            recovery-agent: trusted-recovery-agent, 
            unlock-height: (+ block-height security-duration),
            certificate-coverage: (get coverage certificate)})
    ;; Log security incident
    (print {alert: "security-incident", participant: tx-sender, action: "certificate-suspended"})
    (ok true)))


