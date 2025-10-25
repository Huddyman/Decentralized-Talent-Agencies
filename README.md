# 🎭 Decentralized Talent Agencies

A blockchain-based platform for indie artists to manage bookings and payments through smart contracts on the Stacks blockchain.

## 🌟 Features

- **Artist Registration**: Artists can register with their name, genre, and hourly rate
- **Booking System**: Clients can book artists for specific time periods
- **Escrow Payments**: Secure payment handling with funds held in escrow
- **Rating System**: Clients can rate and review artists after completed bookings
- **Platform Fees**: Configurable platform fee system
- **Status Management**: Artists can toggle availability and update rates

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- [Stacks CLI](https://github.com/blockstack/stacks-cli) (optional)

### Installation

1. Clone the repository
2. Navigate to the project directory
3. Run `clarinet check` to verify the contract

## 📖 Usage

### For Artists 🎨

#### Register as an Artist
```clarity
(contract-call? .Dec-Talent-Agencies register-artist "John Doe" "Jazz" u100)
```

#### Update Your Rate
```clarity
(contract-call? .Dec-Talent-Agencies update-artist-rate u150)
```

#### Toggle Availability
```clarity
(contract-call? .Dec-Talent-Agencies toggle-artist-status)
```

#### Accept a Booking
```clarity
(contract-call? .Dec-Talent-Agencies accept-booking u1)
```

#### Complete a Booking
```clarity
(contract-call? .Dec-Talent-Agencies complete-booking u1)
```

### For Clients 💼

#### Create a Booking
```clarity
;; Book artist ID 1 for 2 hours starting at block 1000
(contract-call? .Dec-Talent-Agencies create-booking u1 u1000 u2)
```

#### Cancel a Booking
```clarity
(contract-call? .Dec-Talent-Agencies cancel-booking u1)
```

#### Leave a Review
```clarity
(contract-call? .Dec-Talent-Agencies leave-review u1 u5 "Amazing performance!")
```

### Query Functions 🔍

#### Get Artist Information
```clarity
(contract-call? .Dec-Talent-Agencies get-artist u1)
```

#### Get Booking Details
```clarity
(contract-call? .Dec-Talent-Agencies get-booking u1)
```

#### Get Review
```clarity
(contract-call? .Dec-Talent-Agencies get-review u1)
```

## 💰 Payment Flow

1. **Booking Creation**: Client pays total amount + platform fee into escrow
2. **Artist Acceptance**: Artist accepts the booking
3. **Service Completion**: Artist marks booking as complete
4. **Payment Release**: Funds are released to artist, platform fee goes to contract owner
5. **Review**: Client can leave a rating and review

## 🏗️ Contract Structure

### Data Maps
- `artists`: Store artist profiles and metadata
- `bookings`: Track all booking information
- `escrow-funds`: Handle payment escrow
- `reviews`: Store client reviews and ratings

### Key Functions
- **register-artist**: Register as a new artist
- **create-booking**: Book an artist's services
- **complete-booking**: Complete a booking and release payments
- **leave-review**: Rate and review an artist

## 🔧 Configuration

### Platform Fee
The contract owner can set platform fees (in basis points, max 100 = 10%):
```clarity
(contract-call? .Dec-Talent-Agencies set-platform-fee u25) ;; 2.5%
```

## 🛡️ Security Features

- **Escrow System**: Payments held securely until service completion
- **Access Control**: Only authorized users can perform specific actions
- **Validation**: Input validation and error handling
- **Status Checks**: Booking status verification before state changes

## 📊 Error Codes

- `u100`: Owner only operation
- `u101`: Record not found
- `u102`: Unauthorized access
- `u103`: Invalid amount
- `u104`: Record already exists
- `u105`: Booking not active
- `u106`: Payment failed
- `u107`: Invalid rating (must be 1-5)
- `u108`: Booking already complete
- `u109`: Insufficient funds

## 🧪 Testing

Run the contract verification:
```bash
clarinet check
```

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Run `clarinet check` to verify
5. Submit a pull request

## 📄 License

This project is open source and available under the [MIT License](LICENSE).

## 🎵 Built for Artists, by Artists

Supporting the indie artist community through decentralized technology! 🎶
