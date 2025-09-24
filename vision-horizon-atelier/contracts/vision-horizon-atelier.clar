;; Vision Horizon Atelier - Creative Rights Management Platform
;; A decentralized platform for managing creative IP and royalty distribution

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_ALREADY_EXISTS (err u101))
(define-constant ERR_NOT_FOUND (err u102))
(define-constant ERR_INSUFFICIENT_FUNDS (err u103))
(define-constant ERR_INVALID_PERCENTAGE (err u104))
(define-constant ERR_INVALID_PRICE (err u105))

;; Data Variables
(define-data-var platform-fee-percentage uint u250) ;; 2.5% in basis points
(define-data-var next-ip-id uint u1)

;; Data Maps
(define-map intellectual-property
  { ip-id: uint }
  {
    creator: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    content-hash: (buff 32),
    creation-timestamp: uint,
    license-price: uint,
    royalty-percentage: uint,
    is-active: bool,
    total-revenue: uint
  }
)

(define-map ip-collaborators
  { ip-id: uint, collaborator: principal }
  { share-percentage: uint, is-active: bool }
)

(define-map user-profiles
  { user: principal }
  {
    display-name: (string-ascii 50),
    reputation-score: uint,
    total-creations: uint,
    is-verified: bool,
    join-timestamp: uint
  }
)

(define-map license-agreements
  { license-id: uint }
  {
    ip-id: uint,
    licensee: principal,
    license-type: (string-ascii 20),
    start-time: uint,
    end-time: uint,
    territory: (string-ascii 50),
    price-paid: uint,
    is-active: bool
  }
)

(define-data-var next-license-id uint u1)

;; Public Functions

;; Register new intellectual property
(define-public (register-ip 
    (title (string-ascii 100))
    (description (string-ascii 500))
    (content-hash (buff 32))
    (license-price uint)
    (royalty-percentage uint))
  (let ((ip-id (var-get next-ip-id)))
    (asserts! (> license-price u0) ERR_INVALID_PRICE)
    (asserts! (<= royalty-percentage u10000) ERR_INVALID_PERCENTAGE)
    (asserts! (is-none (map-get? intellectual-property { ip-id: ip-id })) ERR_ALREADY_EXISTS)
    
    ;; Create IP entry
    (map-set intellectual-property
      { ip-id: ip-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        content-hash: content-hash,
        creation-timestamp: block-height,
        license-price: license-price,
        royalty-percentage: royalty-percentage,
        is-active: true,
        total-revenue: u0
      }
    )
    
    ;; Update user profile
    (match (map-get? user-profiles { user: tx-sender })
      existing-profile
        (map-set user-profiles
          { user: tx-sender }
          (merge existing-profile { total-creations: (+ (get total-creations existing-profile) u1) })
        )
      (map-set user-profiles
        { user: tx-sender }
        {
          display-name: "",
          reputation-score: u100,
          total-creations: u1,
          is-verified: false,
          join-timestamp: block-height
        }
      )
    )
    
    ;; Increment IP counter
    (var-set next-ip-id (+ ip-id u1))
    (ok ip-id)
  )
)

;; Add collaborator to IP
(define-public (add-collaborator (ip-id uint) (collaborator principal) (share-percentage uint))
  (let ((ip-data (unwrap! (map-get? intellectual-property { ip-id: ip-id }) ERR_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get creator ip-data)) ERR_NOT_AUTHORIZED)
    (asserts! (<= share-percentage u10000) ERR_INVALID_PERCENTAGE)
    (asserts! (is-none (map-get? ip-collaborators { ip-id: ip-id, collaborator: collaborator })) ERR_ALREADY_EXISTS)
    
    (map-set ip-collaborators
      { ip-id: ip-id, collaborator: collaborator }
      { share-percentage: share-percentage, is-active: true }
    )
    (ok true)
  )
)

;; Purchase license for IP
(define-public (purchase-license 
    (ip-id uint)
    (license-type (string-ascii 20))
    (duration-blocks uint)
    (territory (string-ascii 50)))
  (let 
    (
      (ip-data (unwrap! (map-get? intellectual-property { ip-id: ip-id }) ERR_NOT_FOUND))
      (license-id (var-get next-license-id))
      (license-price (get license-price ip-data))
      (platform-fee (/ (* license-price (var-get platform-fee-percentage)) u10000))
      (creator-payment (- license-price platform-fee))
    )
    (asserts! (get is-active ip-data) ERR_NOT_FOUND)
    (asserts! (> duration-blocks u0) ERR_INVALID_PRICE)
    
    ;; Transfer payment
    (try! (stx-transfer? license-price tx-sender (as-contract tx-sender)))
    
    ;; Create license agreement
    (map-set license-agreements
      { license-id: license-id }
      {
        ip-id: ip-id,
        licensee: tx-sender,
        license-type: license-type,
        start-time: block-height,
        end-time: (+ block-height duration-blocks),
        territory: territory,
        price-paid: license-price,
        is-active: true
      }
    )
    
    ;; Update IP revenue
    (map-set intellectual-property
      { ip-id: ip-id }
      (merge ip-data { total-revenue: (+ (get total-revenue ip-data) license-price) })
    )
    
    ;; Distribute payment to creator
    (try! (as-contract (stx-transfer? creator-payment tx-sender (get creator ip-data))))
    
    ;; Increment license counter
    (var-set next-license-id (+ license-id u1))
    (ok license-id)
  )
)

;; Distribute royalties to collaborators
(define-public (distribute-royalties (ip-id uint) (amount uint))
  (let ((ip-data (unwrap! (map-get? intellectual-property { ip-id: ip-id }) ERR_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get creator ip-data)) ERR_NOT_AUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_PRICE)
    
    ;; Transfer amount to contract for distribution
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Update total revenue
    (map-set intellectual-property
      { ip-id: ip-id }
      (merge ip-data { total-revenue: (+ (get total-revenue ip-data) amount) })
    )
    
    (ok true)
  )
)

;; Update user profile
(define-public (update-profile (display-name (string-ascii 50)))
  (match (map-get? user-profiles { user: tx-sender })
    existing-profile
      (begin
        (map-set user-profiles
          { user: tx-sender }
          (merge existing-profile { display-name: display-name })
        )
        (ok true)
      )
    (begin
      (map-set user-profiles
        { user: tx-sender }
        {
          display-name: display-name,
          reputation-score: u100,
          total-creations: u0,
          is-verified: false,
          join-timestamp: block-height
        }
      )
      (ok true)
    )
  )
)

;; Verify user (admin function)
(define-public (verify-user (user principal))
  (let ((profile (unwrap! (map-get? user-profiles { user: user }) ERR_NOT_FOUND)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    
    (map-set user-profiles
      { user: user }
      (merge profile { is-verified: true, reputation-score: (+ (get reputation-score profile) u50) })
    )
    (ok true)
  )
)

;; Read-only functions

;; Get IP details
(define-read-only (get-ip-details (ip-id uint))
  (map-get? intellectual-property { ip-id: ip-id })
)

;; Get user profile
(define-read-only (get-user-profile (user principal))
  (map-get? user-profiles { user: user })
)

;; Get collaborator share
(define-read-only (get-collaborator-share (ip-id uint) (collaborator principal))
  (map-get? ip-collaborators { ip-id: ip-id, collaborator: collaborator })
)

;; Get license details
(define-read-only (get-license-details (license-id uint))
  (map-get? license-agreements { license-id: license-id })
)

;; Check if license is active
(define-read-only (is-license-active (license-id uint))
  (match (map-get? license-agreements { license-id: license-id })
    license-data
      (and 
        (get is-active license-data)
        (<= block-height (get end-time license-data))
      )
    false
  )
)

;; Get platform fee percentage
(define-read-only (get-platform-fee)
  (var-get platform-fee-percentage)
)

;; Get next IP ID
(define-read-only (get-next-ip-id)
  (var-get next-ip-id)
)

;; Get next license ID
(define-read-only (get-next-license-id)
  (var-get next-license-id)
)

;; Admin function to update platform fee
(define-public (update-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (<= new-fee u1000) ERR_INVALID_PERCENTAGE) ;; Max 10%
    (var-set platform-fee-percentage new-fee)
    (ok true)
  )
)
