;; Asset Tokenization Smart Contract
;; Summary
;; This smart contract facilitates the tokenization of assets on the Stacks blockchain.
;; It allows for the registration, minting, transfer, and listing of asset tokens,
;; as well as compliance checks and marketplace interactions.

;; Description
;; The contract defines constants for error handling and the contract owner,
;; as well as data variables for tracking total assets and compliance authority.
;; It includes data maps for storing asset details, token balances, marketplace listings, and compliance status.


;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_ASSET_EXISTS (err u101))
(define-constant ERR_ASSET_NOT_FOUND (err u102))
(define-constant ERR_INSUFFICIENT_BALANCE (err u103))
(define-constant ERR_NOT_LISTED (err u104))
(define-constant ERR_INVALID_PRICE (err u105))
(define-constant ERR_COMPLIANCE_CHECK_FAILED (err u106))

;; Data Variables
(define-data-var total-assets uint u0)
(define-data-var compliance-authority principal CONTRACT_OWNER)

;; Data Maps
(define-map assets
    { asset-id: uint }
    {
        owner: principal,
        asset-type: (string-ascii 32),
        metadata-uri: (string-utf8 256),
        total-supply: uint,
        is-frozen: bool
    }
)

(define-map token-balances
    { asset-id: uint, owner: principal }
    { balance: uint }
)

(define-map marketplace-listings
    { asset-id: uint, seller: principal }
    {
        price: uint,
        quantity: uint,
        expiry: uint
    }
)

(define-map compliance-status
    { asset-id: uint, user: principal }
    { approved: bool, timestamp: uint }
)

;; Private Functions
(define-private (is-contract-owner)
    (is-eq tx-sender CONTRACT_OWNER)
)

(define-private (asset-exists (asset-id uint))
    (default-to false
        (match (map-get? assets { asset-id: asset-id })
            asset true
            false
        )
    )
)

(define-private (get-balance (asset-id uint) (user principal))
    (default-to u0
        (get balance
            (map-get? token-balances { asset-id: asset-id, owner: user })
        )
    )
)

(define-private (transfer-tokens (asset-id uint) (from principal) (to principal) (amount uint))
    (let (
        (from-balance (get-balance asset-id from))
        (to-balance (get-balance asset-id to))
    )
    (if (>= from-balance amount)
        (begin
            (map-set token-balances
                { asset-id: asset-id, owner: from }
                { balance: (- from-balance amount) }
            )
            (map-set token-balances
                { asset-id: asset-id, owner: to }
                { balance: (+ to-balance amount) }
            )
            (ok true)
        )
        ERR_INSUFFICIENT_BALANCE
    ))
)

;; Public Functions
(define-public (register-asset (asset-type (string-ascii 32)) (metadata-uri (string-utf8 256)) (initial-supply uint))
    (let ((new-asset-id (+ (var-get total-assets) u1)))
        (if (is-contract-owner)
            (begin
                (map-set assets
                    { asset-id: new-asset-id }
                    {
                        owner: tx-sender,
                        asset-type: asset-type,
                        metadata-uri: metadata-uri,
                        total-supply: initial-supply,
                        is-frozen: false
                    }
                )
                (map-set token-balances
                    { asset-id: new-asset-id, owner: tx-sender }
                    { balance: initial-supply }
                )
                (var-set total-assets new-asset-id)
                (ok new-asset-id)
            )
            ERR_NOT_AUTHORIZED
        )
    )
)

(define-public (mint-tokens (asset-id uint) (amount uint) (recipient principal))
    (let ((asset (unwrap! (map-get? assets { asset-id: asset-id }) ERR_ASSET_NOT_FOUND)))
        (if (and (is-eq (get owner asset) tx-sender) (not (get is-frozen asset)))
            (let (
                (current-balance (get-balance asset-id recipient))
                (new-total-supply (+ (get total-supply asset) amount))
            )
                (map-set assets
                    { asset-id: asset-id }
                    (merge asset { total-supply: new-total-supply })
                )
                (map-set token-balances
                    { asset-id: asset-id, owner: recipient }
                    { balance: (+ current-balance amount) }
                )
                (ok true)
            )
            ERR_NOT_AUTHORIZED
        )
    )
)

(define-public (transfer (asset-id uint) (to principal) (amount uint))
    (let ((asset (unwrap! (map-get? assets { asset-id: asset-id }) ERR_ASSET_NOT_FOUND)))
        (if (not (get is-frozen asset))
            (transfer-tokens asset-id tx-sender to amount)
            ERR_NOT_AUTHORIZED
        )
    )
)

(define-public (list-asset (asset-id uint) (price uint) (quantity uint) (expiry uint))
    (let (
        (asset (unwrap! (map-get? assets { asset-id: asset-id }) ERR_ASSET_NOT_FOUND))
        (seller-balance (get-balance asset-id tx-sender))
    )
        (if (and
                (>= seller-balance quantity)
                (> price u0)
                (>= expiry block-height)
            )
            (begin
                (map-set marketplace-listings
                    { asset-id: asset-id, seller: tx-sender }
                    {
                        price: price,
                        quantity: quantity,
                        expiry: expiry
                    }
                )
                (ok true)
            )
            ERR_INVALID_PRICE
        )
    )
)

(define-public (buy-asset (asset-id uint) (seller principal) (quantity uint))
    (let (
        (listing (unwrap! (map-get? marketplace-listings { asset-id: asset-id, seller: seller }) ERR_NOT_LISTED))
        (total-cost (* (get price listing) quantity))
    )
        (if (and
                (<= quantity (get quantity listing))
                (>= (get expiry listing) block-height)
            )
            (begin
                ;; Transfer STX payment
                (unwrap! (stx-transfer? total-cost tx-sender seller) ERR_INSUFFICIENT_BALANCE)
                ;; Transfer tokens
                (unwrap! (transfer-tokens asset-id seller tx-sender quantity) ERR_INSUFFICIENT_BALANCE)
                ;; Update listing
                (if (is-eq quantity (get quantity listing))
                    (map-delete marketplace-listings { asset-id: asset-id, seller: seller })
                    (map-set marketplace-listings
                        { asset-id: asset-id, seller: seller }
                        (merge listing { quantity: (- (get quantity listing) quantity) })
                    )
                )
                (ok true)
            )
            ERR_NOT_LISTED
        )
    )
)

;; Compliance Functions
(define-public (set-compliance-authority (new-authority principal))
    (if (is-contract-owner)
        (begin
            (var-set compliance-authority new-authority)
            (ok true)
        )
        ERR_NOT_AUTHORIZED
    )
)

(define-public (approve-user (asset-id uint) (user principal))
    (if (is-eq tx-sender (var-get compliance-authority))
        (begin
            (map-set compliance-status
                { asset-id: asset-id, user: user }
                { approved: true, timestamp: block-height }
            )
            (ok true)
        )
        ERR_NOT_AUTHORIZED
    )
)

;; Read-only Functions
(define-read-only (get-asset-info (asset-id uint))
    (map-get? assets { asset-id: asset-id })
)

(define-read-only (get-user-balance (asset-id uint) (user principal))
    (get-balance asset-id user)
)

(define-read-only (get-listing (asset-id uint) (seller principal))
    (map-get? marketplace-listings { asset-id: asset-id, seller: seller })
)

(define-read-only (is-user-approved (asset-id uint) (user principal))
    (default-to
        false
        (get approved (map-get? compliance-status { asset-id: asset-id, user: user }))
    )
)