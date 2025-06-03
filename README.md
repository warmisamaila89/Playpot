# 🎮 Playpot - Game Tournament Pool Contract

A decentralized escrowed tournament system built on Stacks blockchain where players can join tournaments, compete, and winners automatically receive their prize money! 🏆

## 🚀 Features

- 🎯 **Create Tournaments**: Set up tournaments with custom entry fees and player limits
- 💰 **Automatic Escrow**: Entry fees are safely held in the smart contract
- 🏅 **Winner Payouts**: Top 3 winners receive 60%, 30%, and 10% of the prize pool
- 🔒 **Secure Claims**: Winners can claim their prizes after tournament completion
- 📊 **Tournament Tracking**: Full visibility into tournament status and participants

## 🎲 How It Works

### For Tournament Creators
1. **Create Tournament** - Set name, entry fee, max players, and duration
2. **Start Tournament** - Begin the competition when ready
3. **End Tournament** - Close the tournament after the time period
4. **Set Winners** - Declare the top 3 winners to distribute prizes

### For Players
1. **Register** - Pay entry fee to join an open tournament
2. **Compete** - Play your game during the active tournament period
3. **Claim Winnings** - If you win, claim your prize from the contract!

## 🛠️ Contract Functions

### Public Functions
- `create-tournament` - Create a new tournament
- `register-for-tournament` - Join a tournament by paying entry fee
- `start-tournament` - Begin tournament (creator only)
- `end-tournament` - End tournament (creator only)
- `set-winners` - Declare winners and distribute prizes (creator only)
- `claim-winnings` - Claim your winnings if you won

### Read-Only Functions
- `get-tournament` - Get tournament details
- `get-tournament-player` - Check player registration status
- `get-tournament-winner` - Get winner by position
- `get-player-winnings` - Check winnings amount
- `is-player-registered` - Verify if player is registered

## 💎 Prize Distribution

- 🥇 **1st Place**: 60% of prize pool
- 🥈 **2nd Place**: 30% of prize pool  
- 🥉 **3rd Place**: 10% of prize pool

## 🔧 Usage Example

```clarity
;; Create a tournament
(contract-call? .playpot create-tournament "Epic Gaming Tournament" u1000000 u10 u1000)

;; Register for tournament ID 1
(contract-call? .playpot register-for-tournament u1)

;; Start the tournament (creator only)
(contract-call? .playpot start-tournament u1)

;; End the tournament (creator only)
(contract-call? .playpot end-tournament u1)

;; Set winners (creator only)
(contract-call? .playpot set-winners u1 'SP1WINNER 'SP2WINNER 'SP3WINNER)

;; Claim your winnings
(contract-call? .playpot claim-winnings u1)
```

## 🏗️ Development

Built with Clarinet for Stacks blockchain. Deploy and test locally:

```bash
clarinet console
```

## 🎊 Ready to Play?

Deploy this contract and start hosting tournaments with automatic prize distribution! Perfect for gaming communities, esports events, and competitive challenges.

**Let the games begin!** 🎮✨


