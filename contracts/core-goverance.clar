;; NexusDAO Core Governance Contract

;; Define constants
(define-constant CONTRACT-ADMIN tx-sender)
(define-constant ERROR-NOT-AUTHORIZED (err u100))
(define-constant ERROR-INSUFFICIENT-TOKENS (err u101))
(define-constant ERROR-PROPOSAL-NOT-FOUND (err u102))
(define-constant ERROR-INVALID-PROPOSAL-DATA (err u103))
(define-constant ERROR-INVALID-TOKEN-AMOUNT (err u104))
(define-constant ERROR-INVALID-VOTING-DURATION (err u105))
(define-constant ERROR-INVALID-QUORUM-THRESHOLD (err u106))

;; Proposal Status Constants
(define-constant STATUS-PENDING u0)
(define-constant STATUS-ACTIVE u1)
(define-constant STATUS-PASSED u2)
(define-constant STATUS-REJECTED u3)
(define-constant STATUS-EXECUTED u4)

;; Governance Parameters
(define-constant MAX-PROPOSAL-DESCRIPTION-LENGTH u500)
(define-constant MAX-FUNCTION-NAME-LENGTH u100)
(define-constant MIN-VOTING-DURATION-BLOCKS u10)
(define-constant MAX-VOTING-DURATION-BLOCKS u1000)
(define-constant MAX-TOKEN-MINT_AMOUNT u10000)
(define-constant MAX-QUORUM_THRESHOLD u1000000)

;; DAO Proposal Structure
(define-map GovernanceProposals
  { proposal-id: uint }
  {
    creator: principal,
    description: (string-utf8 500),
    voting-start-block: uint,
    voting-end-block: uint,
    status: uint,
    votes-in-favor: uint,
    votes-against: uint,
    quorum-requirement: uint,
    target-contract: (optional principal),
    executable-function: (optional (string-utf8 100))
  }
)

;; Governance Token Map
(define-map MemberTokenBalance
  principal
  uint
)

;; Track Next Proposal ID
(define-data-var proposal-counter uint u0)

;; Validation Functions
(define-private (is-valid-description (description-text (string-utf8 500)))
  (and 
    (> (len description-text) u0)
    (<= (len description-text) MAX-PROPOSAL-DESCRIPTION-LENGTH)
  )
)

(define-private (is-valid-voting-duration (duration uint))
  (and 
    (>= duration MIN-VOTING-DURATION-BLOCKS)
    (<= duration MAX-VOTING-DURATION-BLOCKS)
  )
)

(define-private (is-valid-token-amount (token-amount uint))
  (and 
    (> token-amount u0)
    (<= token-amount MAX-TOKEN-MINT_AMOUNT)
  )
)

(define-private (is-valid-quorum (quorum-threshold uint))
  (and 
    (> quorum-threshold u0)
    (<= quorum-threshold MAX-QUORUM_THRESHOLD)
  )
)

(define-private (is-valid-target-contract (target-contract (optional principal)))
  (match target-contract
    some-contract (not (is-eq some-contract tx-sender))
    true
  )
)

(define-private (is-valid-function-name (function-name (optional (string-utf8 100))))
  (match function-name
    some-func (and 
      (> (len some-func) u0)
      (<= (len some-func) MAX-FUNCTION-NAME-LENGTH)
    )
    true
  )
)

;; Read-only functions to get proposal and token balance
(define-read-only (get-proposal-details (proposal-id uint))
  (map-get? GovernanceProposals { proposal-id: proposal-id })
)

(define-read-only (get-member-token-balance (member-address principal))
  (default-to u0 (map-get? MemberTokenBalance member-address))
)

;; Mint Governance Tokens
(define-public (mint-governance-tokens (token-amount uint) (recipient-address principal))
  (begin
    ;; Validate inputs
    (asserts! (is-admin tx-sender) ERROR-NOT-AUTHORIZED)
    (asserts! (is-valid-token-amount token-amount) ERROR-INVALID-TOKEN-AMOUNT)
    (asserts! (not (is-eq recipient-address tx-sender)) ERROR-NOT-AUTHORIZED)
    
    ;; Mint tokens
    (map-set MemberTokenBalance 
      recipient-address 
      (+ (get-member-token-balance recipient-address) token-amount)
    )
    (ok token-amount)
  )
)

;; Create a new DAO Proposal
(define-public (create-proposal 
  (description-text (string-utf8 500))
  (voting-duration uint)
  (quorum-threshold uint)
  (target-contract (optional principal))
  (function-name (optional (string-utf8 100)))
)
  (let 
    (
      (proposal-id (var-get proposal-counter))
      (current-block-height block-height)
    )
    ;; Validate inputs
    (asserts! (is-valid-description description-text) ERROR-INVALID-PROPOSAL-DATA)
    (asserts! (is-valid-voting-duration voting-duration) ERROR-INVALID-VOTING-DURATION)
    (asserts! (is-valid-quorum quorum-threshold) ERROR-INVALID-QUORUM-THRESHOLD)
    (asserts! (is-valid-target-contract target-contract) ERROR-NOT-AUTHORIZED)
    (asserts! (is-valid-function-name function-name) ERROR-INVALID-PROPOSAL-DATA)
    (asserts! (> (get-member-token-balance tx-sender) u0) ERROR-NOT-AUTHORIZED)
    
    ;; Create proposal mapping
    (map-set GovernanceProposals 
      { proposal-id: proposal-id }
      {
        creator: tx-sender,
        description: description-text,
        voting-start-block: current-block-height,
        voting-end-block: (+ current-block-height voting-duration),
        status: STATUS-PENDING,
        votes-in-favor: u0,
        votes-against: u0,
        quorum-requirement: quorum-threshold,
        target-contract: target-contract,
        executable-function: function-name
      }
    )
    
    ;; Increment proposal ID
    (var-set proposal-counter (+ proposal-id u1))
    
    (ok proposal-id)
  )
)
