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
(define-constant ERR_BRACKET_NOT_INITIALIZED (err u112))
(define-constant ERR_MATCH_NOT_FOUND (err u113))
(define-constant ERR_MATCH_ALREADY_COMPLETED (err u114))
(define-constant ERR_NOT_PARTICIPANT (err u115))
(define-constant ERR_ROUND_NOT_COMPLETE (err u116))
(define-constant ERR_INVALID_BRACKET_SIZE (err u117))

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

(define-map tournament-brackets
  uint
  {
    total-players: uint,
    total-rounds: uint,
    current-round: uint,
    bracket-initialized: bool,
    bracket-active: bool
  }
)

(define-map bracket-matches
  { tournament-id: uint, round: uint, match-id: uint }
  {
    player1: (optional principal),
    player2: (optional principal),
    winner: (optional principal),
    match-completed: bool,
    bye-match: bool
  }
)

(define-map player-bracket-position
  { tournament-id: uint, player: principal }
  { round: uint, match-id: uint, position: uint }
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

(define-private (calculate-rounds (players uint))
  (if (is-eq players u1)
    u0
    (if (<= players u2)
      u1
      (if (<= players u4)
        u2
        (if (<= players u8)
          u3
          (if (<= players u16)
            u4
            (if (<= players u32)
              u5
              (if (<= players u64)
                u6
                u7
              )
            )
          )
        )
      )
    )
  )
)

(define-private (is-power-of-two (n uint))
  (if (is-eq n u0)
    false
    (is-eq (mod n u2) u0)
  )
)

(define-private (next-power-of-two (n uint))
  (if (<= n u1) u1
    (if (<= n u2) u2
      (if (<= n u4) u4
        (if (<= n u8) u8
          (if (<= n u16) u16
            (if (<= n u32) u32
              (if (<= n u64) u64
                u128
              )
            )
          )
        )
      )
    )
  )
)

(define-public (initialize-bracket (tournament-id uint))
  (let
    (
      (tournament (unwrap! (map-get? tournaments tournament-id) ERR_TOURNAMENT_NOT_FOUND))
      (current-players (get current-players tournament))
      (bracket-size (next-power-of-two current-players))
      (total-rounds (calculate-rounds bracket-size))
      (matches-in-first-round (/ bracket-size u2))
    )
    (asserts! (is-eq tx-sender (get creator tournament)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status tournament) "active") ERR_TOURNAMENT_NOT_ACTIVE)
    (asserts! (>= current-players u2) ERR_INVALID_BRACKET_SIZE)
    (asserts! (is-none (map-get? tournament-brackets tournament-id)) ERR_TOURNAMENT_ALREADY_EXISTS)
    
    (map-set tournament-brackets tournament-id
      {
        total-players: current-players,
        total-rounds: total-rounds,
        current-round: u1,
        bracket-initialized: true,
        bracket-active: true
      }
    )
    
    (try! (setup-first-round tournament-id current-players matches-in-first-round))
    (ok true)
  )
)

(define-private (setup-first-round (tournament-id uint) (total-players uint) (matches-count uint))
  (let
    (
      (bracket-size (next-power-of-two total-players))
      (byes-needed (- bracket-size total-players))
    )
    (create-bracket-matches tournament-id u1 matches-count byes-needed)
  )
)

(define-private (create-bracket-matches (tournament-id uint) (round uint) (total-matches uint) (byes-needed uint))
  (let
    (
      (match-id u1)
    )
    (if (<= total-matches u8)
      (begin
        (and 
          (>= total-matches u1) 
          (map-set bracket-matches { tournament-id: tournament-id, round: round, match-id: u1 }
            { player1: none, player2: (if (> byes-needed u0) none (some tx-sender)), winner: none, match-completed: (> byes-needed u0), bye-match: (> byes-needed u0) }))
        (and 
          (>= total-matches u2) 
          (map-set bracket-matches { tournament-id: tournament-id, round: round, match-id: u2 }
            { player1: none, player2: (if (> byes-needed u1) none (some tx-sender)), winner: none, match-completed: (> byes-needed u1), bye-match: (> byes-needed u1) }))
        (and 
          (>= total-matches u3) 
          (map-set bracket-matches { tournament-id: tournament-id, round: round, match-id: u3 }
            { player1: none, player2: (if (> byes-needed u2) none (some tx-sender)), winner: none, match-completed: (> byes-needed u2), bye-match: (> byes-needed u2) }))
        (and 
          (>= total-matches u4) 
          (map-set bracket-matches { tournament-id: tournament-id, round: round, match-id: u4 }
            { player1: none, player2: (if (> byes-needed u3) none (some tx-sender)), winner: none, match-completed: (> byes-needed u3), bye-match: (> byes-needed u3) }))
        (and 
          (>= total-matches u5) 
          (map-set bracket-matches { tournament-id: tournament-id, round: round, match-id: u5 }
            { player1: none, player2: (if (> byes-needed u4) none (some tx-sender)), winner: none, match-completed: (> byes-needed u4), bye-match: (> byes-needed u4) }))
        (and 
          (>= total-matches u6) 
          (map-set bracket-matches { tournament-id: tournament-id, round: round, match-id: u6 }
            { player1: none, player2: (if (> byes-needed u5) none (some tx-sender)), winner: none, match-completed: (> byes-needed u5), bye-match: (> byes-needed u5) }))
        (and 
          (>= total-matches u7) 
          (map-set bracket-matches { tournament-id: tournament-id, round: round, match-id: u7 }
            { player1: none, player2: (if (> byes-needed u6) none (some tx-sender)), winner: none, match-completed: (> byes-needed u6), bye-match: (> byes-needed u6) }))
        (and 
          (>= total-matches u8) 
          (map-set bracket-matches { tournament-id: tournament-id, round: round, match-id: u8 }
            { player1: none, player2: (if (> byes-needed u7) none (some tx-sender)), winner: none, match-completed: (> byes-needed u7), bye-match: (> byes-needed u7) }))
        (ok true)
      )
      ERR_INVALID_BRACKET_SIZE
    )
  )
)

(define-public (assign-player-to-bracket (tournament-id uint) (player principal) (round uint) (match-id uint))
  (let
    (
      (tournament (unwrap! (map-get? tournaments tournament-id) ERR_TOURNAMENT_NOT_FOUND))
      (bracket (unwrap! (map-get? tournament-brackets tournament-id) ERR_BRACKET_NOT_INITIALIZED))
      (match-data (unwrap! (map-get? bracket-matches { tournament-id: tournament-id, round: round, match-id: match-id }) ERR_MATCH_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get creator tournament)) ERR_NOT_AUTHORIZED)
    (asserts! (get bracket-active bracket) ERR_TOURNAMENT_NOT_ACTIVE)
    (asserts! (is-some (map-get? tournament-players { tournament-id: tournament-id, player: player })) ERR_NOT_REGISTERED)
    
    (if (is-none (get player1 match-data))
      (begin
        (map-set bracket-matches
          { tournament-id: tournament-id, round: round, match-id: match-id }
          (merge match-data { player1: (some player) })
        )
        (map-set player-bracket-position
          { tournament-id: tournament-id, player: player }
          { round: round, match-id: match-id, position: u1 }
        )
        (ok true)
      )
      (if (is-none (get player2 match-data))
        (begin
          (map-set bracket-matches
            { tournament-id: tournament-id, round: round, match-id: match-id }
            (merge match-data { player2: (some player) })
          )
          (map-set player-bracket-position
            { tournament-id: tournament-id, player: player }
            { round: round, match-id: match-id, position: u2 }
          )
          (ok true)
        )
        ERR_ALREADY_REGISTERED
      )
    )
  )
)

(define-public (report-match-result (tournament-id uint) (round uint) (match-id uint) (winner principal))
  (let
    (
      (tournament (unwrap! (map-get? tournaments tournament-id) ERR_TOURNAMENT_NOT_FOUND))
      (bracket (unwrap! (map-get? tournament-brackets tournament-id) ERR_BRACKET_NOT_INITIALIZED))
      (match-data (unwrap! (map-get? bracket-matches { tournament-id: tournament-id, round: round, match-id: match-id }) ERR_MATCH_NOT_FOUND))
      (player1 (get player1 match-data))
      (player2 (get player2 match-data))
    )
    (asserts! (is-eq tx-sender (get creator tournament)) ERR_NOT_AUTHORIZED)
    (asserts! (get bracket-active bracket) ERR_TOURNAMENT_NOT_ACTIVE)
    (asserts! (not (get match-completed match-data)) ERR_MATCH_ALREADY_COMPLETED)
    (asserts! (is-eq (get current-round bracket) round) ERR_TOURNAMENT_NOT_ACTIVE)
    (asserts! (or 
      (is-eq (some winner) player1) 
      (is-eq (some winner) player2)
    ) ERR_NOT_PARTICIPANT)
    
    (map-set bracket-matches
      { tournament-id: tournament-id, round: round, match-id: match-id }
      (merge match-data { 
        winner: (some winner), 
        match-completed: true 
      })
    )
    (ok true)
  )
)

(define-public (advance-to-next-round (tournament-id uint))
  (let
    (
      (tournament (unwrap! (map-get? tournaments tournament-id) ERR_TOURNAMENT_NOT_FOUND))
      (bracket (unwrap! (map-get? tournament-brackets tournament-id) ERR_BRACKET_NOT_INITIALIZED))
      (current-round (get current-round bracket))
      (total-rounds (get total-rounds bracket))
    )
    (asserts! (is-eq tx-sender (get creator tournament)) ERR_NOT_AUTHORIZED)
    (asserts! (get bracket-active bracket) ERR_TOURNAMENT_NOT_ACTIVE)
    (asserts! (< current-round total-rounds) ERR_TOURNAMENT_ALREADY_STARTED)
    (asserts! (is-eq (check-round-complete tournament-id current-round) true) ERR_ROUND_NOT_COMPLETE)
    
    (let
      (
        (next-round (+ current-round u1))
        (matches-in-next-round (/ (get-matches-in-round tournament-id current-round) u2))
      )
      (map-set tournament-brackets tournament-id
        (merge bracket { current-round: next-round })
      )
      
      (if (< next-round total-rounds)
        (try! (setup-next-round tournament-id next-round matches-in-next-round))
        (try! (finalize-bracket tournament-id))
      )
      (ok true)
    )
  )
)

(define-private (check-round-complete (tournament-id uint) (round uint))
  (let
    (
      (matches-in-round (get-matches-in-round tournament-id round))
    )
    (verify-all-matches-complete tournament-id round matches-in-round)
  )
)

(define-private (verify-all-matches-complete (tournament-id uint) (round uint) (total-matches uint))
  (and 
    (or (<= total-matches u0) (is-match-complete tournament-id round u1))
    (or (<= total-matches u1) (is-match-complete tournament-id round u2))
    (or (<= total-matches u2) (is-match-complete tournament-id round u3))
    (or (<= total-matches u3) (is-match-complete tournament-id round u4))
    (or (<= total-matches u4) (is-match-complete tournament-id round u5))
    (or (<= total-matches u5) (is-match-complete tournament-id round u6))
    (or (<= total-matches u6) (is-match-complete tournament-id round u7))
    (or (<= total-matches u7) (is-match-complete tournament-id round u8))
  )
)

(define-private (is-match-complete (tournament-id uint) (round uint) (match-id uint))
  (let
    (
      (match-data (map-get? bracket-matches { tournament-id: tournament-id, round: round, match-id: match-id }))
    )
    (if (is-some match-data)
      (get match-completed (unwrap-panic match-data))
      true
    )
  )
)

(define-private (get-matches-in-round (tournament-id uint) (round uint))
  (let
    (
      (bracket (unwrap-panic (map-get? tournament-brackets tournament-id)))
      (total-players (get total-players bracket))
      (bracket-size (next-power-of-two total-players))
    )
    (/ bracket-size (pow u2 round))
  )
)

(define-private (setup-next-round (tournament-id uint) (round uint) (matches-count uint))
  (build-next-round-matches tournament-id round matches-count)
)

(define-private (build-next-round-matches (tournament-id uint) (round uint) (total-matches uint))
  (let
    (
      (prev-round (- round u1))
    )
    (if (<= total-matches u4)
      (begin
        (and 
          (>= total-matches u1)
          (let
            (
              (winner1 (get-match-winner tournament-id prev-round u1))
              (winner2 (get-match-winner tournament-id prev-round u2))
            )
            (map-set bracket-matches { tournament-id: tournament-id, round: round, match-id: u1 }
              { player1: winner1, player2: winner2, winner: none, match-completed: false, bye-match: false })
          ))
        (and 
          (>= total-matches u2)
          (let
            (
              (winner1 (get-match-winner tournament-id prev-round u3))
              (winner2 (get-match-winner tournament-id prev-round u4))
            )
            (map-set bracket-matches { tournament-id: tournament-id, round: round, match-id: u2 }
              { player1: winner1, player2: winner2, winner: none, match-completed: false, bye-match: false })
          ))
        (and 
          (>= total-matches u3)
          (let
            (
              (winner1 (get-match-winner tournament-id prev-round u5))
              (winner2 (get-match-winner tournament-id prev-round u6))
            )
            (map-set bracket-matches { tournament-id: tournament-id, round: round, match-id: u3 }
              { player1: winner1, player2: winner2, winner: none, match-completed: false, bye-match: false })
          ))
        (and 
          (>= total-matches u4)
          (let
            (
              (winner1 (get-match-winner tournament-id prev-round u7))
              (winner2 (get-match-winner tournament-id prev-round u8))
            )
            (map-set bracket-matches { tournament-id: tournament-id, round: round, match-id: u4 }
              { player1: winner1, player2: winner2, winner: none, match-completed: false, bye-match: false })
          ))
        (ok true)
      )
      ERR_INVALID_BRACKET_SIZE
    )
  )
)

(define-private (get-match-winner (tournament-id uint) (round uint) (match-id uint))
  (let
    (
      (match-data (map-get? bracket-matches { tournament-id: tournament-id, round: round, match-id: match-id }))
    )
    (if (is-some match-data)
      (get winner (unwrap-panic match-data))
      none
    )
  )
)

(define-private (finalize-bracket (tournament-id uint))
  (let
    (
      (bracket (unwrap! (map-get? tournament-brackets tournament-id) ERR_BRACKET_NOT_INITIALIZED))
      (final-round (get total-rounds bracket))
      (champion (get-match-winner tournament-id final-round u1))
    )
    (map-set tournament-brackets tournament-id
      (merge bracket { bracket-active: false })
    )
    
    (if (is-some champion)
      (set-winners tournament-id (unwrap-panic champion) none none)
      (ok true)
    )
  )
)

(define-read-only (get-tournament-bracket (tournament-id uint))
  (map-get? tournament-brackets tournament-id)
)

(define-read-only (get-bracket-match (tournament-id uint) (round uint) (match-id uint))
  (map-get? bracket-matches { tournament-id: tournament-id, round: round, match-id: match-id })
)

(define-read-only (get-player-bracket-position (tournament-id uint) (player principal))
  (map-get? player-bracket-position { tournament-id: tournament-id, player: player })
)

(define-read-only (get-bracket-champion (tournament-id uint))
  (let
    (
      (bracket (map-get? tournament-brackets tournament-id))
    )
    (match bracket
      bracket-data
      (let
        (
          (final-round (get total-rounds bracket-data))
        )
        (get-match-winner tournament-id final-round u1)
      )
      none
    )
  )
)