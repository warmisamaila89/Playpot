(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_TOURNAMENT_NOT_FOUND (err u101))
(define-constant ERR_TOURNAMENT_ALREADY_EXISTS (err u102))
(define-constant ERR_TOURNAMENT_NOT_ACTIVE (err u103))
(define-constant ERR_TOURNAMENT_ALREADY_STARTED (err u104))
(define-constant ERR_TOURNAMENT_NOT_ENDED (err u105))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u106))
(define-constant ERR_ALREADY_REGISTERED (err u107))
(define-constant ERR_NOT_REGISTERED (err u108))
(define-constant ERR_INVALID_WINNER (err u109))
(define-constant ERR_ALREADY_CLAIMED (err u110))
(define-constant ERR_NO_WINNINGS (err u111))

(define-data-var next-tournament-id uint u1)

(define-map tournaments
  uint
  {
    name: (string-ascii 50),
    entry-fee: uint,
    max-players: uint,
    current-players: uint,
    status: (string-ascii 20),
    creator: principal,
    prize-pool: uint,
    start-block: uint,
    end-block: uint
  }
)

(define-map tournament-players
  { tournament-id: uint, player: principal }
  { registered: bool, claimed: bool }
)

(define-map tournament-winners
  { tournament-id: uint, position: uint }
  principal
)

(define-map player-winnings
  { tournament-id: uint, player: principal }
  uint
)

(define-public (create-tournament (name (string-ascii 50)) (entry-fee uint) (max-players uint) (duration-blocks uint))
  (let
    (
      (tournament-id (var-get next-tournament-id))
      (start-block (+ stacks-block-height u10))
      (end-block (+ start-block duration-blocks))
    )
    (asserts! (> max-players u1) ERR_NOT_AUTHORIZED)
    (asserts! (> entry-fee u0) ERR_NOT_AUTHORIZED)
    (map-set tournaments tournament-id
      {
        name: name,
        entry-fee: entry-fee,
        max-players: max-players,
        current-players: u0,
        status: "open",
        creator: tx-sender,
        prize-pool: u0,
        start-block: start-block,
        end-block: end-block
      }
    )
    (var-set next-tournament-id (+ tournament-id u1))
    (ok tournament-id)
  )
)

(define-public (register-for-tournament (tournament-id uint))
  (let
    (
      (tournament (unwrap! (map-get? tournaments tournament-id) ERR_TOURNAMENT_NOT_FOUND))
      (entry-fee (get entry-fee tournament))
      (current-players (get current-players tournament))
      (max-players (get max-players tournament))
      (status (get status tournament))
    )
    (asserts! (is-eq status "open") ERR_TOURNAMENT_NOT_ACTIVE)
    (asserts! (< current-players max-players) ERR_TOURNAMENT_ALREADY_STARTED)
    (asserts! (is-none (map-get? tournament-players { tournament-id: tournament-id, player: tx-sender })) ERR_ALREADY_REGISTERED)
    
    (try! (stx-transfer? entry-fee tx-sender (as-contract tx-sender)))
    
    (map-set tournament-players 
      { tournament-id: tournament-id, player: tx-sender }
      { registered: true, claimed: false }
    )
    
    (map-set tournaments tournament-id
      (merge tournament {
        current-players: (+ current-players u1),
        prize-pool: (+ (get prize-pool tournament) entry-fee)
      })
    )
    
    (ok true)
  )
)

(define-public (start-tournament (tournament-id uint))
  (let
    (
      (tournament (unwrap! (map-get? tournaments tournament-id) ERR_TOURNAMENT_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get creator tournament)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status tournament) "open") ERR_TOURNAMENT_ALREADY_STARTED)
    (asserts! (>= stacks-block-height (get start-block tournament)) ERR_NOT_AUTHORIZED)
    
    (map-set tournaments tournament-id
      (merge tournament { status: "active" })
    )
    (ok true)
  )
)

(define-public (end-tournament (tournament-id uint))
  (let
    (
      (tournament (unwrap! (map-get? tournaments tournament-id) ERR_TOURNAMENT_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get creator tournament)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status tournament) "active") ERR_TOURNAMENT_NOT_ACTIVE)
    (asserts! (>= stacks-block-height (get end-block tournament)) ERR_NOT_AUTHORIZED)
    
    (map-set tournaments tournament-id
      (merge tournament { status: "ended" })
    )
    (ok true)
  )
)

(define-public (set-winners (tournament-id uint) (first-place principal) (second-place (optional principal)) (third-place (optional principal)))
  (let
    (
      (tournament (unwrap! (map-get? tournaments tournament-id) ERR_TOURNAMENT_NOT_FOUND))
      (prize-pool (get prize-pool tournament))
      (first-prize (/ (* prize-pool u60) u100))
      (second-prize (/ (* prize-pool u30) u100))
      (third-prize (/ (* prize-pool u10) u100))
    )
    (asserts! (is-eq tx-sender (get creator tournament)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status tournament) "ended") ERR_TOURNAMENT_NOT_ENDED)
    (asserts! (is-some (map-get? tournament-players { tournament-id: tournament-id, player: first-place })) ERR_INVALID_WINNER)
    
    (map-set tournament-winners { tournament-id: tournament-id, position: u1 } first-place)
    (map-set player-winnings { tournament-id: tournament-id, player: first-place } first-prize)
    
    (match second-place
      second-winner
      (begin
        (asserts! (is-some (map-get? tournament-players { tournament-id: tournament-id, player: second-winner })) ERR_INVALID_WINNER)
        (map-set tournament-winners { tournament-id: tournament-id, position: u2 } second-winner)
        (map-set player-winnings { tournament-id: tournament-id, player: second-winner } second-prize)
      )
      true
    )
    
    (match third-place
      third-winner
      (begin
        (asserts! (is-some (map-get? tournament-players { tournament-id: tournament-id, player: third-winner })) ERR_INVALID_WINNER)
        (map-set tournament-winners { tournament-id: tournament-id, position: u3 } third-winner)
        (map-set player-winnings { tournament-id: tournament-id, player: third-winner } third-prize)
      )
      true
    )
    
    (map-set tournaments tournament-id
      (merge tournament { status: "completed" })
    )
    (ok true)
  )
)

(define-public (claim-winnings (tournament-id uint))
  (let
    (
      (tournament (unwrap! (map-get? tournaments tournament-id) ERR_TOURNAMENT_NOT_FOUND))
      (player-data (unwrap! (map-get? tournament-players { tournament-id: tournament-id, player: tx-sender }) ERR_NOT_REGISTERED))
      (winnings (default-to u0 (map-get? player-winnings { tournament-id: tournament-id, player: tx-sender })))
    )
    (asserts! (is-eq (get status tournament) "completed") ERR_TOURNAMENT_NOT_ENDED)
    (asserts! (not (get claimed player-data)) ERR_ALREADY_CLAIMED)
    (asserts! (> winnings u0) ERR_NO_WINNINGS)
    
    (try! (as-contract (stx-transfer? winnings tx-sender tx-sender)))
    
    (map-set tournament-players 
      { tournament-id: tournament-id, player: tx-sender }
      (merge player-data { claimed: true })
    )
    (ok winnings)
  )
)

(define-read-only (get-tournament (tournament-id uint))
  (map-get? tournaments tournament-id)
)

(define-read-only (get-tournament-player (tournament-id uint) (player principal))
  (map-get? tournament-players { tournament-id: tournament-id, player: player })
)

(define-read-only (get-tournament-winner (tournament-id uint) (position uint))
  (map-get? tournament-winners { tournament-id: tournament-id, position: position })
)

(define-read-only (get-player-winnings (tournament-id uint) (player principal))
  (map-get? player-winnings { tournament-id: tournament-id, player: player })
)

(define-read-only (get-next-tournament-id)
  (var-get next-tournament-id)
)

(define-read-only (is-player-registered (tournament-id uint) (player principal))
  (is-some (map-get? tournament-players { tournament-id: tournament-id, player: player }))
)

(define-read-only (get-contract-balance)
  (stx-get-balance (as-contract tx-sender))
)