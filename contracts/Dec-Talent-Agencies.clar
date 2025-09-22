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
(define-constant err-portfolio-limit-exceeded (err u110))
(define-constant err-portfolio-item-not-found (err u111))
(define-constant err-time-slot-conflict (err u112))
(define-constant err-invalid-time-slot (err u113))
(define-constant err-artist-not-available (err u114))

;; data vars
(define-data-var next-artist-id uint u1)
(define-data-var next-booking-id uint u1)
(define-data-var platform-fee uint u50)
(define-data-var next-portfolio-id uint u1)
(define-data-var max-portfolio-items uint u10)
(define-data-var next-availability-id uint u1)

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

(define-map portfolio-items
  { portfolio-id: uint }
  {
    artist-id: uint,
    title: (string-ascii 128),
    description: (string-ascii 512),
    media-url: (string-ascii 256),
    media-type: (string-ascii 32),
    created-at: uint
  }
)

(define-map artist-portfolio-count
  { artist-id: uint }
  { count: uint }
)

(define-map availability-slots
  { availability-id: uint }
  {
    artist-id: uint,
    start-time: uint,
    end-time: uint,
    is-available: bool,
    created-at: uint
  }
)

(define-map artist-booked-slots
  { artist-id: uint, start-time: uint }
  {
    end-time: uint,
    booking-id: uint
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
    
    (map-set artist-portfolio-count
      { artist-id: artist-id }
      { count: u0 }
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
      (end-time (+ start-time duration))
    )
    (asserts! (get is-active artist-data) err-booking-not-active)
    (asserts! (> duration u0) err-invalid-amount)
    (asserts! (> start-time stacks-block-height) err-invalid-amount)
    (asserts! (is-artist-available artist-id start-time end-time) err-artist-not-available)
    (asserts! (not (has-booking-conflict artist-id start-time end-time)) err-time-slot-conflict)
    
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
      (start-time (get start-time booking-data))
      (end-time (+ start-time (get duration booking-data)))
      (artist-id (get artist-id booking-data))
    )
    (asserts! (is-eq tx-sender (get owner artist-data)) err-unauthorized)
    (asserts! (is-eq (get status booking-data) "pending") err-booking-not-active)
    
    (map-set artist-booked-slots
      { artist-id: artist-id, start-time: start-time }
      { end-time: end-time, booking-id: booking-id }
    )
    
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
    
    (map-delete artist-booked-slots { artist-id: (get artist-id booking-data), start-time: (get start-time booking-data) })
    (map-delete escrow-funds { booking-id: booking-id })
    (ok true)
  )
)

(define-public (cancel-booking (booking-id uint))
  (let
    (
      (booking-data (unwrap! (map-get? bookings { booking-id: booking-id }) err-not-found))
      (escrow-data (unwrap! (map-get? escrow-funds { booking-id: booking-id }) err-not-found))
      (artist-id (get artist-id booking-data))
      (start-time (get start-time booking-data))
    )
    (asserts! (is-eq tx-sender (get client booking-data)) err-unauthorized)
    (asserts! (is-eq (get status booking-data) "pending") err-booking-not-active)
    
    (try! (as-contract (stx-transfer? (get amount escrow-data) tx-sender (get client booking-data))))
    
    (if (is-eq (get status booking-data) "accepted")
      (map-delete artist-booked-slots { artist-id: artist-id, start-time: start-time })
      true
    )
    
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

(define-public (add-portfolio-item (title (string-ascii 128)) (description (string-ascii 512)) (media-url (string-ascii 256)) (media-type (string-ascii 32)))
  (let
    (
      (artist-record (unwrap! (map-get? artist-by-owner { owner: tx-sender }) err-not-found))
      (artist-id (get artist-id artist-record))
      (portfolio-id (var-get next-portfolio-id))
      (current-count-data (default-to { count: u0 } (map-get? artist-portfolio-count { artist-id: artist-id })))
      (current-count (get count current-count-data))
    )
    (asserts! (< current-count (var-get max-portfolio-items)) err-portfolio-limit-exceeded)
    
    (map-set portfolio-items
      { portfolio-id: portfolio-id }
      {
        artist-id: artist-id,
        title: title,
        description: description,
        media-url: media-url,
        media-type: media-type,
        created-at: stacks-block-height
      }
    )
    
    (map-set artist-portfolio-count
      { artist-id: artist-id }
      { count: (+ current-count u1) }
    )
    
    (var-set next-portfolio-id (+ portfolio-id u1))
    (ok portfolio-id)
  )
)

(define-public (remove-portfolio-item (portfolio-id uint))
  (let
    (
      (portfolio-data (unwrap! (map-get? portfolio-items { portfolio-id: portfolio-id }) err-portfolio-item-not-found))
      (artist-record (unwrap! (map-get? artist-by-owner { owner: tx-sender }) err-not-found))
      (artist-id (get artist-id artist-record))
      (current-count-data (default-to { count: u0 } (map-get? artist-portfolio-count { artist-id: artist-id })))
      (current-count (get count current-count-data))
    )
    (asserts! (is-eq (get artist-id portfolio-data) artist-id) err-unauthorized)
    
    (map-delete portfolio-items { portfolio-id: portfolio-id })
    
    (map-set artist-portfolio-count
      { artist-id: artist-id }
      { count: (if (> current-count u0) (- current-count u1) u0) }
    )
    (ok true)
  )
)

(define-public (update-portfolio-limit (new-limit uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> new-limit u0) err-invalid-amount)
    (var-set max-portfolio-items new-limit)
    (ok true)
  )
)

(define-public (set-availability (start-time uint) (end-time uint) (is-available bool))
  (let
    (
      (artist-record (unwrap! (map-get? artist-by-owner { owner: tx-sender }) err-not-found))
      (artist-id (get artist-id artist-record))
      (availability-id (var-get next-availability-id))
    )
    (asserts! (> end-time start-time) err-invalid-time-slot)
    (asserts! (> start-time stacks-block-height) err-invalid-time-slot)
    
    (map-set availability-slots
      { availability-id: availability-id }
      {
        artist-id: artist-id,
        start-time: start-time,
        end-time: end-time,
        is-available: is-available,
        created-at: stacks-block-height
      }
    )
    
    (var-set next-availability-id (+ availability-id u1))
    (ok availability-id)
  )
)

(define-public (update-availability (availability-id uint) (is-available bool))
  (let
    (
      (availability-data (unwrap! (map-get? availability-slots { availability-id: availability-id }) err-not-found))
      (artist-record (unwrap! (map-get? artist-by-owner { owner: tx-sender }) err-not-found))
      (artist-id (get artist-id artist-record))
    )
    (asserts! (is-eq (get artist-id availability-data) artist-id) err-unauthorized)
    
    (map-set availability-slots
      { availability-id: availability-id }
      (merge availability-data { is-available: is-available })
    )
    (ok true)
  )
)

(define-public (remove-availability (availability-id uint))
  (let
    (
      (availability-data (unwrap! (map-get? availability-slots { availability-id: availability-id }) err-not-found))
      (artist-record (unwrap! (map-get? artist-by-owner { owner: tx-sender }) err-not-found))
      (artist-id (get artist-id artist-record))
    )
    (asserts! (is-eq (get artist-id availability-data) artist-id) err-unauthorized)
    
    (map-delete availability-slots { availability-id: availability-id })
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

(define-read-only (get-portfolio-item (portfolio-id uint))
  (map-get? portfolio-items { portfolio-id: portfolio-id })
)

(define-read-only (get-artist-portfolio-count (artist-id uint))
  (default-to { count: u0 } (map-get? artist-portfolio-count { artist-id: artist-id }))
)

(define-read-only (get-max-portfolio-items)
  (var-get max-portfolio-items)
)

(define-read-only (get-next-portfolio-id)
  (var-get next-portfolio-id)
)

(define-read-only (is-artist-available (artist-id uint) (start-time uint) (end-time uint))
  true
)

(define-read-only (has-booking-conflict (artist-id uint) (start-time uint) (end-time uint))
  (is-some (map-get? artist-booked-slots { artist-id: artist-id, start-time: start-time }))
)

(define-read-only (get-availability-slot (availability-id uint))
  (map-get? availability-slots { availability-id: availability-id })
)

(define-read-only (get-artist-booked-slot (artist-id uint) (start-time uint))
  (map-get? artist-booked-slots { artist-id: artist-id, start-time: start-time })
)

(define-read-only (get-next-availability-id)
  (var-get next-availability-id)
)

(define-read-only (check-booking-availability (artist-id uint) (start-time uint) (duration uint))
  (let
    (
      (end-time (+ start-time duration))
      (is-available (is-artist-available artist-id start-time end-time))
      (has-conflict (has-booking-conflict artist-id start-time end-time))
    )
    {
      is-available: is-available,
      has-conflict: has-conflict,
      can-book: (and is-available (not has-conflict)),
      start-time: start-time,
      end-time: end-time
    }
  )
)

(define-read-only (get-artist-schedule-status (artist-id uint) (start-time uint) (end-time uint))
  (let
    (
      (availability-check (is-artist-available artist-id start-time end-time))
      (conflict-check (has-booking-conflict artist-id start-time end-time))
    )
    {
      artist-id: artist-id,
      time-slot: { start: start-time, end: end-time },
      is-available: availability-check,
      has-booking-conflict: conflict-check,
      can-accept-booking: (and availability-check (not conflict-check))
    }
  )
)
