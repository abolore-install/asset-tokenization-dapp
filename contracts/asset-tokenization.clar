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
(define-constant ERR_INVALID_PARAMS (err u107))
(define-constant ERR_INVALID_STRING (err u108))
(define-constant ERR_INVALID_EXPIRY (err u109))
(define-constant ERR_INVALID_RECIPIENT (err u110))
(define-constant ERR_SELF_TRANSFER (err u111))
(define-constant ERR_MARKETPLACE_FROZEN (err u112))
(define-constant ERR_SELF_TRADE (err u113))
(define-constant ERR_INVALID_AUTHORITY (err u114))

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
    (match (map-get? assets { asset-id: asset-id })
        asset true
        false
    )
)

(define-private (validate-asset-id (asset-id uint))
    (and 
        (> asset-id u0)
        (<= asset-id (var-get total-assets))
    )
)

(define-private (validate-amount (amount uint))
    (> amount u0)
)

(define-private (validate-string-ascii (str (string-ascii 32)))
    (and 
        (> (len str) u0)
        (<= (len str) u32)
    )
)

(define-private (validate-string-utf8 (str (string-utf8 256)))
    (and 
        (> (len str) u0)
        (<= (len str) u256)
    )
)

(define-private (validate-expiry (expiry uint))
    (>= expiry block-height)
)

(define-private (get-balance-or-zero (asset-id uint) (user principal))
    (default-to u0
        (get balance
            (map-get? token-balances { asset-id: asset-id, owner: user })
        )
    )
)

(define-private (get-balance (asset-id uint) (user principal))
    (ok (get-balance-or-zero asset-id user))
)

(define-private (transfer-tokens (asset-id uint) (from principal) (to principal) (amount uint))
    (let (
        (from-balance (get-balance-or-zero asset-id from))
        (to-balance (get-balance-or-zero asset-id to))
    )
    (asserts! (validate-asset-id asset-id) ERR_ASSET_NOT_FOUND)
    (asserts! (validate-amount amount) ERR_INVALID_PARAMS)
    (asserts! (>= from-balance amount) ERR_INSUFFICIENT_BALANCE)
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
    ))
)

(define-private (validate-recipient (recipient principal) (asset-id uint))
    (begin
        (asserts! (not (is-eq recipient (as-contract tx-sender))) ERR_INVALID_RECIPIENT)
        (asserts! (asset-exists asset-id) ERR_ASSET_NOT_FOUND)
        (ok true)
    )
)

(define-private (validate-marketplace-status (asset-id uint) (seller principal))
    (ok (and 
        (asset-exists asset-id)
        (not (is-eq seller (as-contract tx-sender)))
        (match (map-get? assets { asset-id: asset-id })
            asset (not (get is-frozen asset))
            false
        )
    ))
)

(define-private (validate-authority (new-authority principal) (current-authority principal))
    (ok (and 
        (not (is-eq new-authority (as-contract tx-sender)))
        (not (is-eq new-authority current-authority))
    ))
)

(define-private (validate-principal (user principal))
    (ok (and
        (not (is-eq user (as-contract tx-sender)))
        (not (is-eq user CONTRACT_OWNER))
    ))
)

;; Public Functions
(define-public (register-asset (asset-type (string-ascii 32)) (metadata-uri (string-utf8 256)) (initial-supply uint))
    (let ((new-asset-id (+ (var-get total-assets) u1)))
        (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
        (asserts! (validate-string-ascii asset-type) ERR_INVALID_STRING)
        (asserts! (validate-string-utf8 metadata-uri) ERR_INVALID_STRING)
        (asserts! (validate-amount initial-supply) ERR_INVALID_PARAMS)
        (asserts! (not (asset-exists new-asset-id)) ERR_ASSET_EXISTS)
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
)

(define-public (mint-tokens (asset-id uint) (amount uint) (recipient principal))
    (let ((asset (unwrap! (map-get? assets { asset-id: asset-id }) ERR_ASSET_NOT_FOUND)))
        (asserts! (validate-asset-id asset-id) ERR_ASSET_NOT_FOUND)
        (asserts! (validate-amount amount) ERR_INVALID_PARAMS)
        (asserts! (unwrap! (validate-recipient recipient asset-id) ERR_INVALID_RECIPIENT) ERR_INVALID_RECIPIENT)
        (asserts! (and (is-eq (get owner asset) tx-sender) (not (get is-frozen asset))) ERR_NOT_AUTHORIZED)
        (let (
            (current-balance (get-balance-or-zero asset-id recipient))
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
    )
)

(define-public (transfer (asset-id uint) (to principal) (amount uint))
    (let ((asset (unwrap! (map-get? assets { asset-id: asset-id }) ERR_ASSET_NOT_FOUND)))
        (asserts! (validate-asset-id asset-id) ERR_ASSET_NOT_FOUND)
        (asserts! (validate-amount amount) ERR_INVALID_PARAMS)
        (asserts! (unwrap! (validate-recipient to asset-id) ERR_INVALID_RECIPIENT) ERR_INVALID_RECIPIENT)
        (asserts! (not (is-eq tx-sender to)) ERR_SELF_TRANSFER)
        (asserts! (not (get is-frozen asset)) ERR_NOT_AUTHORIZED)
        (transfer-tokens asset-id tx-sender to amount)
    )
)

(define-public (list-asset (asset-id uint) (price uint) (quantity uint) (expiry uint))
    (let (
        (asset (unwrap! (map-get? assets { asset-id: asset-id }) ERR_ASSET_NOT_FOUND))
        (seller-balance (get-balance-or-zero asset-id tx-sender))
    )
        (asserts! (validate-asset-id asset-id) ERR_ASSET_NOT_FOUND)
        (asserts! (validate-amount price) ERR_INVALID_PRICE)
        (asserts! (validate-amount quantity) ERR_INVALID_PARAMS)
        (asserts! (validate-expiry expiry) ERR_INVALID_EXPIRY)
        (asserts! (>= seller-balance quantity) ERR_INSUFFICIENT_BALANCE)
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
    )
)

(define-public (buy-asset (asset-id uint) (seller principal) (quantity uint))
    (let (
        (listing (unwrap! (map-get? marketplace-listings { asset-id: asset-id, seller: seller }) ERR_NOT_LISTED))
    )
        (asserts! (validate-asset-id asset-id) ERR_ASSET_NOT_FOUND)
        (asserts! (validate-amount quantity) ERR_INVALID_PARAMS)
        (asserts! (unwrap! (validate-marketplace-status asset-id seller) ERR_MARKETPLACE_FROZEN) ERR_MARKETPLACE_FROZEN)
        (asserts! (not (is-eq tx-sender seller)) ERR_SELF_TRADE)
        (asserts! (<= quantity (get quantity listing)) ERR_INSUFFICIENT_BALANCE)
        (asserts! (validate-expiry (get expiry listing)) ERR_INVALID_EXPIRY)
        
        (let ((total-cost (* (get price listing) quantity)))
            (try! (stx-transfer? total-cost tx-sender seller))
            (try! (transfer-tokens asset-id seller tx-sender quantity))
            (if (is-eq quantity (get quantity listing))
                (map-delete marketplace-listings { asset-id: asset-id, seller: seller })
                (map-set marketplace-listings
                    { asset-id: asset-id, seller: seller }
                    (merge listing { quantity: (- (get quantity listing) quantity) })
                )
            )
            (ok true)
        )
    )
)

;; Compliance Functions
(define-public (set-compliance-authority (new-authority principal))
    (begin
        (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
        (asserts! 
            (unwrap! 
                (validate-authority new-authority (var-get compliance-authority)) 
                ERR_INVALID_AUTHORITY
            ) 
            ERR_INVALID_AUTHORITY
        )
        (var-set compliance-authority new-authority)
        (ok true)
    )
)

(define-public (approve-user (asset-id uint) (user principal))
    (begin
        (asserts! (validate-asset-id asset-id) ERR_ASSET_NOT_FOUND)
        (asserts! (is-eq tx-sender (var-get compliance-authority)) ERR_NOT_AUTHORIZED)
        (asserts! (unwrap! (validate-principal user) ERR_INVALID_PARAMS) ERR_INVALID_PARAMS)
        (map-set compliance-status
            { asset-id: asset-id, user: user }
            { approved: true, timestamp: block-height }
        )
        (ok true)
    )
)

;; Read-only Functions
(define-read-only (get-asset-info (asset-id uint))
    (if (validate-asset-id asset-id)
        (ok (unwrap-panic (map-get? assets { asset-id: asset-id })))
        ERR_ASSET_NOT_FOUND)
)

(define-read-only (get-user-balance (asset-id uint) (user principal))
    (begin
        (asserts! (validate-asset-id asset-id) ERR_ASSET_NOT_FOUND)
        (asserts! (unwrap! (validate-principal user) ERR_INVALID_PARAMS) ERR_INVALID_PARAMS)
        (ok (get-balance-or-zero asset-id user))
    )
)

(define-read-only (get-listing (asset-id uint) (seller principal))
    (if (validate-asset-id asset-id)
        (ok (map-get? marketplace-listings { asset-id: asset-id, seller: seller }))
        ERR_ASSET_NOT_FOUND)
)

(define-read-only (is-user-approved (asset-id uint) (user principal))
    (if (validate-asset-id asset-id)
        (ok (default-to
            false
            (get approved (map-get? compliance-status { asset-id: asset-id, user: user }))))
        ERR_ASSET_NOT_FOUND)
)
