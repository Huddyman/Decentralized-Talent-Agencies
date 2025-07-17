;; title: Dec-Talent-Agencies
;; version: 1.0
;; summary: Decentralized talent agencies platform for indie artists
;; description: Smart contracts handle bookings/payments for indie artists with escrow and rating system

;; constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-already-exists (err u104))
(define-constant err-booking-not-active (err u105))
(define-constant err-payment-failed (err u106))
(define-constant err-invalid-rating (err u107))
(define-constant err-booking-complete (err u108))
(define-constant err-insufficient-funds (err u109))

;; data vars
(define-data-var next-artist-id uint u1)
(define-data-var next-booking-id uint u1)
(define-data-var platform-fee uint u50)

;; data maps
(define-map artists
  { artist-id: uint }
  {
    owner: principal,
    name: (string-ascii 64),
    genre: (string-ascii 32),
    rate-per-hour: uint,
    total-bookings: uint,
    rating: uint,
    rating-count: uint,
    is-active: bool,
    created-at: uint
  }
)

(define-map artist-by-owner
  { owner: principal }
  { artist-id: uint }
)

(define-map bookings
  { booking-id: uint }
  {
    artist-id: uint,
    client: principal,
    start-time: uint,
    duration: uint,
    total-amount: uint,
    escrow-amount: uint,
    status: (string-ascii 16),
    created-at: uint,
    completed-at: (optional uint)
  }
)

(define-map escrow-funds
  { booking-id: uint }
  { amount: uint }
)

(define-map reviews
  { booking-id: uint }
  {
    rating: uint,
    review: (string-ascii 256),
    reviewer: principal,
    created-at: uint
  }
)

;; public functions
(define-public (register-artist (name (string-ascii 64)) (genre (string-ascii 32)) (rate-per-hour uint))
  (let
    (
      (artist-id (var-get next-artist-id))
      (caller tx-sender)
    )
    (asserts! (is-none (map-get? artist-by-owner { owner: caller })) err-already-exists)
    (asserts! (> rate-per-hour u0) err-invalid-amount)
    
    (map-set artists
      { artist-id: artist-id }
      {
        owner: caller,
        name: name,
        genre: genre,
        rate-per-hour: rate-per-hour,
        total-bookings: u0,
        rating: u0,
        rating-count: u0,
        is-active: true,
        created-at: stacks-block-height
      }
    )
    
    (map-set artist-by-owner
      { owner: caller }
      { artist-id: artist-id }
    )
    
    (var-set next-artist-id (+ artist-id u1))
    (ok artist-id)
  )
)

(define-public (update-artist-rate (new-rate uint))
  (let
    (
      (artist-record (unwrap! (map-get? artist-by-owner { owner: tx-sender }) err-not-found))
      (artist-id (get artist-id artist-record))
      (artist-data (unwrap! (map-get? artists { artist-id: artist-id }) err-not-found))
    )
    (asserts! (> new-rate u0) err-invalid-amount)
    
    (map-set artists
      { artist-id: artist-id }
      (merge artist-data { rate-per-hour: new-rate })
    )
    (ok true)
  )
)

(define-public (toggle-artist-status)
  (let
    (
      (artist-record (unwrap! (map-get? artist-by-owner { owner: tx-sender }) err-not-found))
      (artist-id (get artist-id artist-record))
      (artist-data (unwrap! (map-get? artists { artist-id: artist-id }) err-not-found))
    )
    (map-set artists
      { artist-id: artist-id }
      (merge artist-data { is-active: (not (get is-active artist-data)) })
    )
    (ok true)
  )
)

(define-public (create-booking (artist-id uint) (start-time uint) (duration uint))
  (let
    (
      (booking-id (var-get next-booking-id))
      (artist-data (unwrap! (map-get? artists { artist-id: artist-id }) err-not-found))
      (total-amount (* (get rate-per-hour artist-data) duration))
      (escrow-amount (+ total-amount (/ (* total-amount (var-get platform-fee)) u1000)))
      (caller tx-sender)
    )
    (asserts! (get is-active artist-data) err-booking-not-active)
    (asserts! (> duration u0) err-invalid-amount)
    (asserts! (> start-time stacks-block-height) err-invalid-amount)
    
    (try! (stx-transfer? escrow-amount caller (as-contract tx-sender)))
    
    (map-set bookings
      { booking-id: booking-id }
      {
        artist-id: artist-id,
        client: caller,
        start-time: start-time,
        duration: duration,
        total-amount: total-amount,
        escrow-amount: escrow-amount,
        status: "pending",
        created-at: stacks-block-height,
        completed-at: none
      }
    )
    
    (map-set escrow-funds
      { booking-id: booking-id }
      { amount: escrow-amount }
    )
    
    (var-set next-booking-id (+ booking-id u1))
    (ok booking-id)
  )
)

(define-public (accept-booking (booking-id uint))
  (let
    (
      (booking-data (unwrap! (map-get? bookings { booking-id: booking-id }) err-not-found))
      (artist-data (unwrap! (map-get? artists { artist-id: (get artist-id booking-data) }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get owner artist-data)) err-unauthorized)
    (asserts! (is-eq (get status booking-data) "pending") err-booking-not-active)
    
    (map-set bookings
      { booking-id: booking-id }
      (merge booking-data { status: "accepted" })
    )
    (ok true)
  )
)

(define-public (complete-booking (booking-id uint))
  (let
    (
      (booking-data (unwrap! (map-get? bookings { booking-id: booking-id }) err-not-found))
      (artist-data (unwrap! (map-get? artists { artist-id: (get artist-id booking-data) }) err-not-found))
      (escrow-data (unwrap! (map-get? escrow-funds { booking-id: booking-id }) err-not-found))
      (total-amount (get total-amount booking-data))
      (platform-fee-amount (/ (* total-amount (var-get platform-fee)) u1000))
      (artist-payment (- total-amount platform-fee-amount))
    )
    (asserts! (is-eq tx-sender (get owner artist-data)) err-unauthorized)
    (asserts! (is-eq (get status booking-data) "accepted") err-booking-not-active)
    (asserts! (>= stacks-block-height (get start-time booking-data)) err-booking-not-active)
    
    (try! (as-contract (stx-transfer? artist-payment tx-sender (get owner artist-data))))
    (try! (as-contract (stx-transfer? platform-fee-amount tx-sender contract-owner)))
    
    (map-set bookings
      { booking-id: booking-id }
      (merge booking-data { 
        status: "completed",
        completed-at: (some stacks-block-height)
      })
    )
    
    (map-set artists
      { artist-id: (get artist-id booking-data) }
      (merge artist-data { total-bookings: (+ (get total-bookings artist-data) u1) })
    )
    
    (map-delete escrow-funds { booking-id: booking-id })
    (ok true)
  )
)

(define-public (cancel-booking (booking-id uint))
  (let
    (
      (booking-data (unwrap! (map-get? bookings { booking-id: booking-id }) err-not-found))
      (escrow-data (unwrap! (map-get? escrow-funds { booking-id: booking-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get client booking-data)) err-unauthorized)
    (asserts! (is-eq (get status booking-data) "pending") err-booking-not-active)
    
    (try! (as-contract (stx-transfer? (get amount escrow-data) tx-sender (get client booking-data))))
    
    (map-set bookings
      { booking-id: booking-id }
      (merge booking-data { status: "cancelled" })
    )
    
    (map-delete escrow-funds { booking-id: booking-id })
    (ok true)
  )
)

(define-public (leave-review (booking-id uint) (rating uint) (review (string-ascii 256)))
  (let
    (
      (booking-data (unwrap! (map-get? bookings { booking-id: booking-id }) err-not-found))
      (artist-data (unwrap! (map-get? artists { artist-id: (get artist-id booking-data) }) err-not-found))
      (new-rating-count (+ (get rating-count artist-data) u1))
      (current-total-rating (* (get rating artist-data) (get rating-count artist-data)))
      (new-total-rating (+ current-total-rating rating))
      (new-average-rating (/ new-total-rating new-rating-count))
    )
    (asserts! (is-eq tx-sender (get client booking-data)) err-unauthorized)
    (asserts! (is-eq (get status booking-data) "completed") err-booking-complete)
    (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-rating)
    (asserts! (is-none (map-get? reviews { booking-id: booking-id })) err-already-exists)
    
    (map-set reviews
      { booking-id: booking-id }
      {
        rating: rating,
        review: review,
        reviewer: tx-sender,
        created-at: stacks-block-height
      }
    )
    
    (map-set artists
      { artist-id: (get artist-id booking-data) }
      (merge artist-data { 
        rating: new-average-rating,
        rating-count: new-rating-count
      })
    )
    (ok true)
  )
)

(define-public (set-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee u100) err-invalid-amount)
    (var-set platform-fee new-fee)
    (ok true)
  )
)

;; read only functions
(define-read-only (get-artist (artist-id uint))
  (map-get? artists { artist-id: artist-id })
)

(define-read-only (get-artist-by-owner (owner principal))
  (match (map-get? artist-by-owner { owner: owner })
    artist-record (map-get? artists { artist-id: (get artist-id artist-record) })
    none
  )
)

(define-read-only (get-booking (booking-id uint))
  (map-get? bookings { booking-id: booking-id })
)

(define-read-only (get-review (booking-id uint))
  (map-get? reviews { booking-id: booking-id })
)

(define-read-only (get-platform-fee)
  (var-get platform-fee)
)

(define-read-only (get-next-artist-id)
  (var-get next-artist-id)
)

(define-read-only (get-next-booking-id)
  (var-get next-booking-id)
)

(define-read-only (get-contract-owner)
  contract-owner
)
