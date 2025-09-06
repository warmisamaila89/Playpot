;; Tournament Sponsorship System
;; Enables external sponsors to contribute to tournaments and receive promotional benefits

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_SPONSOR_NOT_FOUND (err u201))
(define-constant ERR_TOURNAMENT_NOT_FOUND (err u202))
(define-constant ERR_INSUFFICIENT_CONTRIBUTION (err u203))
(define-constant ERR_SPONSORSHIP_ALREADY_EXISTS (err u204))
(define-constant ERR_TOURNAMENT_NOT_OPEN (err u205))
(define-constant ERR_INVALID_SPONSOR_TIER (err u206))
(define-constant ERR_SPONSOR_LIMIT_REACHED (err u207))
(define-constant ERR_INVALID_PROMOTION_DURATION (err u208))

(define-data-var next-sponsorship-id uint u1)
(define-data-var minimum-sponsor-contribution uint u100000) ;; 0.1 STX minimum
(define-data-var maximum-sponsors-per-tournament uint u5)

;; Store sponsor information
(define-map sponsors
    principal
    {
        company-name: (string-ascii 50),
        contact-info: (string-ascii 100),
        website-url: (string-ascii 100),
        logo-hash: (buff 32),
        verified: bool,
        registration-date: uint,
        total-contributions: uint,
        tournaments-sponsored: uint,
        tier-level: uint
    }
)

;; Track sponsorships for specific tournaments
(define-map tournament-sponsorships
    { tournament-id: uint, sponsor: principal }
    {
        sponsorship-id: uint,
        contribution-amount: uint,
        sponsor-tier: uint, ;; 1=Bronze, 2=Silver, 3=Gold, 4=Platinum
        promotional-message: (string-ascii 200),
        logo-display-duration: uint,
        benefits-claimed: bool,
        contribution-date: uint,
        tournament-visibility: bool
    }
)

;; Store sponsor tier benefits and requirements
(define-map sponsor-tiers
    uint
    {
        tier-name: (string-ascii 20),
        minimum-contribution: uint,
        logo-display-blocks: uint,
        promotional-message-length: uint,
        tournament-promotion: bool,
        leaderboard-visibility: bool,
        custom-rewards: uint
    }
)

;; Track tournament sponsor counts and total contributions
(define-map tournament-sponsor-summary
    uint
    {
        total-sponsors: uint,
        total-sponsor-contributions: uint,
        bronze-sponsors: uint,
        silver-sponsors: uint,
        gold-sponsors: uint,
        platinum-sponsors: uint,
        enhanced-prize-pool: uint
    }
)

;; Initialize sponsor tier system
(define-public (initialize-sponsor-tiers)
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        
        ;; Bronze tier
        (map-set sponsor-tiers u1 {
            tier-name: "Bronze",
            minimum-contribution: u100000,
            logo-display-blocks: u100,
            promotional-message-length: u50,
            tournament-promotion: false,
            leaderboard-visibility: false,
            custom-rewards: u0
        })
        
        ;; Silver tier  
        (map-set sponsor-tiers u2 {
            tier-name: "Silver",
            minimum-contribution: u500000,
            logo-display-blocks: u300,
            promotional-message-length: u100,
            tournament-promotion: true,
            leaderboard-visibility: false,
            custom-rewards: u0
        })
        
        ;; Gold tier
        (map-set sponsor-tiers u3 {
            tier-name: "Gold", 
            minimum-contribution: u1000000,
            logo-display-blocks: u500,
            promotional-message-length: u150,
            tournament-promotion: true,
            leaderboard-visibility: true,
            custom-rewards: u50000
        })
        
        ;; Platinum tier
        (map-set sponsor-tiers u4 {
            tier-name: "Platinum",
            minimum-contribution: u2000000,
            logo-display-blocks: u1000,
            promotional-message-length: u200,
            tournament-promotion: true,
            leaderboard-visibility: true,
            custom-rewards: u100000
        })
        
        (ok true)
    )
)

;; Register as a tournament sponsor
(define-public (register-sponsor
    (company-name (string-ascii 50))
    (contact-info (string-ascii 100))
    (website-url (string-ascii 100))
    (logo-hash (buff 32)))
    (let
        (
            (existing-sponsor (map-get? sponsors tx-sender))
        )
        (asserts! (is-none existing-sponsor) ERR_SPONSORSHIP_ALREADY_EXISTS)
        
        (map-set sponsors tx-sender {
            company-name: company-name,
            contact-info: contact-info,
            website-url: website-url,
            logo-hash: logo-hash,
            verified: false,
            registration-date: stacks-block-height,
            total-contributions: u0,
            tournaments-sponsored: u0,
            tier-level: u1
        })
        
        (ok true)
    )
)

;; Sponsor a tournament with contribution
(define-public (sponsor-tournament
    (tournament-id uint)
    (contribution-amount uint)
    (promotional-message (string-ascii 200)))
    (let
        (
            (tournament (unwrap! (contract-call? .Playpot get-tournament tournament-id) ERR_TOURNAMENT_NOT_FOUND))
            (sponsor-info (unwrap! (map-get? sponsors tx-sender) ERR_SPONSOR_NOT_FOUND))
            (sponsorship-id (var-get next-sponsorship-id))
            (min-contribution (var-get minimum-sponsor-contribution))
            (existing-sponsorship (map-get? tournament-sponsorships { tournament-id: tournament-id, sponsor: tx-sender }))
            (current-sponsor-count (get-tournament-sponsor-count tournament-id))
        )
        (asserts! (is-none existing-sponsorship) ERR_SPONSORSHIP_ALREADY_EXISTS)
        (asserts! (>= contribution-amount min-contribution) ERR_INSUFFICIENT_CONTRIBUTION)
        (asserts! (< current-sponsor-count (var-get maximum-sponsors-per-tournament)) ERR_SPONSOR_LIMIT_REACHED)
        (asserts! (get verified sponsor-info) ERR_UNAUTHORIZED)
        
        (let ((sponsor-tier (calculate-sponsor-tier contribution-amount)))
            (asserts! (> sponsor-tier u0) ERR_INVALID_SPONSOR_TIER)
            
            ;; Transfer contribution to tournament contract
            (try! (stx-transfer? contribution-amount tx-sender (as-contract tx-sender)))
            
            ;; Record sponsorship
            (map-set tournament-sponsorships { tournament-id: tournament-id, sponsor: tx-sender } {
                sponsorship-id: sponsorship-id,
                contribution-amount: contribution-amount,
                sponsor-tier: sponsor-tier,
                promotional-message: promotional-message,
                logo-display-duration: (get-tier-logo-duration sponsor-tier),
                benefits-claimed: false,
                contribution-date: stacks-block-height,
                tournament-visibility: true
            })
            
            ;; Update sponsor statistics
            (map-set sponsors tx-sender
                (merge sponsor-info {
                    total-contributions: (+ (get total-contributions sponsor-info) contribution-amount),
                    tournaments-sponsored: (+ (get tournaments-sponsored sponsor-info) u1),
                    tier-level: (if (> sponsor-tier (get tier-level sponsor-info)) sponsor-tier (get tier-level sponsor-info))
                }))
            
            ;; Update tournament sponsor summary
            (unwrap-panic (update-tournament-sponsor-summary tournament-id contribution-amount sponsor-tier))
            
            (var-set next-sponsorship-id (+ sponsorship-id u1))
            (ok sponsorship-id)
        )
    )
)

;; Verify a sponsor (admin only)
(define-public (verify-sponsor (sponsor principal))
    (let
        (
            (sponsor-info (unwrap! (map-get? sponsors sponsor) ERR_SPONSOR_NOT_FOUND))
        )
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        
        (map-set sponsors sponsor
            (merge sponsor-info { verified: true }))
        (ok true)
    )
)

;; Calculate sponsor tier based on contribution amount
(define-private (calculate-sponsor-tier (contribution uint))
    (if (>= contribution u2000000) u4 ;; Platinum
        (if (>= contribution u1000000) u3 ;; Gold
            (if (>= contribution u500000) u2 ;; Silver
                (if (>= contribution u100000) u1 ;; Bronze
                    u0)))) ;; Invalid
)

;; Get logo display duration for tier
(define-private (get-tier-logo-duration (tier uint))
    (match (map-get? sponsor-tiers tier)
        tier-data (get logo-display-blocks tier-data)
        u100) ;; Default duration
)

;; Count sponsors for a tournament
(define-private (get-tournament-sponsor-count (tournament-id uint))
    (match (map-get? tournament-sponsor-summary tournament-id)
        summary-data (get total-sponsors summary-data)
        u0)
)

;; Update tournament sponsor summary
(define-private (update-tournament-sponsor-summary (tournament-id uint) (contribution uint) (tier uint))
    (let
        (
            (current-summary (default-to 
                {
                    total-sponsors: u0,
                    total-sponsor-contributions: u0,
                    bronze-sponsors: u0,
                    silver-sponsors: u0,
                    gold-sponsors: u0,
                    platinum-sponsors: u0,
                    enhanced-prize-pool: u0
                }
                (map-get? tournament-sponsor-summary tournament-id)
            ))
        )
        (let
            (
                (new-total-sponsors (+ (get total-sponsors current-summary) u1))
                (new-total-contributions (+ (get total-sponsor-contributions current-summary) contribution))
                (new-bronze (if (is-eq tier u1) (+ (get bronze-sponsors current-summary) u1) (get bronze-sponsors current-summary)))
                (new-silver (if (is-eq tier u2) (+ (get silver-sponsors current-summary) u1) (get silver-sponsors current-summary)))
                (new-gold (if (is-eq tier u3) (+ (get gold-sponsors current-summary) u1) (get gold-sponsors current-summary)))
                (new-platinum (if (is-eq tier u4) (+ (get platinum-sponsors current-summary) u1) (get platinum-sponsors current-summary)))
                (enhanced-pool (+ (get enhanced-prize-pool current-summary) contribution))
            )
            (map-set tournament-sponsor-summary tournament-id {
                total-sponsors: new-total-sponsors,
                total-sponsor-contributions: new-total-contributions,
                bronze-sponsors: new-bronze,
                silver-sponsors: new-silver,
                gold-sponsors: new-gold,
                platinum-sponsors: new-platinum,
                enhanced-prize-pool: enhanced-pool
            })
        )
        (ok true)
    )
)

;; Read-only functions
(define-read-only (get-sponsor-info (sponsor principal))
    (map-get? sponsors sponsor)
)

(define-read-only (get-tournament-sponsorship (tournament-id uint) (sponsor principal))
    (map-get? tournament-sponsorships { tournament-id: tournament-id, sponsor: sponsor })
)

(define-read-only (get-sponsor-tier-info (tier uint))
    (map-get? sponsor-tiers tier)
)

(define-read-only (get-tournament-sponsor-summary (tournament-id uint))
    (map-get? tournament-sponsor-summary tournament-id)
)

(define-read-only (calculate-enhanced-prize-distribution (tournament-id uint))
    (let
        (
            (tournament (contract-call? .Playpot get-tournament tournament-id))
            (sponsor-summary (map-get? tournament-sponsor-summary tournament-id))
        )
        (match tournament
            tournament-data 
                (match sponsor-summary
                    summary-data
                        (let
                            (
                                (base-pool (get prize-pool tournament-data))
                                (sponsor-pool (get total-sponsor-contributions summary-data))
                                (total-enhanced-pool (+ base-pool sponsor-pool))
                            )
                            {
                                base-prize-pool: base-pool,
                                sponsor-contributions: sponsor-pool,
                                enhanced-total-pool: total-enhanced-pool,
                                first-place-prize: (/ (* total-enhanced-pool u60) u100),
                                second-place-prize: (/ (* total-enhanced-pool u30) u100),
                                third-place-prize: (/ (* total-enhanced-pool u10) u100)
                            }
                        )
                    {
                        base-prize-pool: (get prize-pool tournament-data),
                        sponsor-contributions: u0,
                        enhanced-total-pool: (get prize-pool tournament-data),
                        first-place-prize: (/ (* (get prize-pool tournament-data) u60) u100),
                        second-place-prize: (/ (* (get prize-pool tournament-data) u30) u100),
                        third-place-prize: (/ (* (get prize-pool tournament-data) u10) u100)
                    }
                )
            {
                base-prize-pool: u0,
                sponsor-contributions: u0,
                enhanced-total-pool: u0,
                first-place-prize: u0,
                second-place-prize: u0,
                third-place-prize: u0
            }
        )
    )
)

(define-read-only (get-sponsor-promotional-value (sponsor principal) (tournament-id uint))
    (match (map-get? tournament-sponsorships { tournament-id: tournament-id, sponsor: sponsor })
        sponsorship-data
            (let
                (
                    (tier (get sponsor-tier sponsorship-data))
                    (contribution (get contribution-amount sponsorship-data))
                    (tier-benefits (unwrap-panic (map-get? sponsor-tiers tier)))
                )
                {
                    sponsor-tier: tier,
                    logo-display-blocks: (get logo-display-blocks tier-benefits),
                    promotional-reach: (calculate-promotional-reach tier contribution),
                    tournament-promotion: (get tournament-promotion tier-benefits),
                    leaderboard-visibility: (get leaderboard-visibility tier-benefits)
                }
            )
        {
            sponsor-tier: u0,
            logo-display-blocks: u0,
            promotional-reach: u0,
            tournament-promotion: false,
            leaderboard-visibility: false
        }
    )
)

;; Calculate promotional reach based on tier and contribution
(define-private (calculate-promotional-reach (tier uint) (contribution uint))
    (let
        (
            (base-reach (* tier u1000))
            (contribution-bonus (/ contribution u10000))
        )
        (+ base-reach contribution-bonus)
    )
)

(define-read-only (get-sponsorship-statistics)
    {
        total-sponsorships: (var-get next-sponsorship-id),
        minimum-contribution: (var-get minimum-sponsor-contribution),
        max-sponsors-per-tournament: (var-get maximum-sponsors-per-tournament),
        total-verified-sponsors: u0 ;; Simplified for demo
    }
)

(define-read-only (get-next-sponsorship-id)
    (var-get next-sponsorship-id)
)
