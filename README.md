# Decentralized Disaster Relief Fund

A transparent and efficient smart contract system for managing disaster relief funds on the Stacks blockchain.

## Overview

The Decentralized Disaster Relief Fund (DDRF) is a Clarity smart contract that enables:

- Transparent collection of donations for specific disaster events
- Secure disbursement of funds to verified relief organizations
- Complete traceability of all donations and disbursements
- Public access to fund allocation and usage data

## Contract Features

- **Disaster Registration**: Authorized administrators can register new disaster events
- **Organization Verification**: Relief organizations are verified before receiving funds
- **Transparent Donations**: All donations are recorded on-chain with full traceability
- **Controlled Disbursements**: Funds are disbursed based on need assessments
- **Public Accountability**: All transactions and fund movements are publicly verifiable

## How to Use

### For Donors

To donate to a specific disaster relief effort:

1. Identify the disaster ID from the public registry
2. Call the `donate-to-disaster` function with the disaster ID and donation amount
3. Your donation will be recorded and allocated to the specified disaster

Example:
```clarity
(contract-call? .decentralized-disaster-relief-fund donate-to-disaster u1 u1000000)
```

### For Administrators

Administrators can:

1. Register new disasters using `register-disaster`
2. Register verified relief organizations using `register-organization`
3. Disburse funds to organizations using `disburse-funds`
4. Close disaster relief efforts using `close-disaster`

### For the Public

Anyone can view:

1. Disaster details using `get-disaster-details`
2. Organization information using `get-organization-details`
3. Donation records using `get-donation-details`
4. Disbursement records using `get-disbursement-details`
5. Total donations and disbursements using `get-total-donations` and `get-total-disbursements`

## Contract Deployment

To deploy this contract using Clarinet:

1. Create a new Clarinet project: `clarinet new decentralized-disaster-relief-fund`
2. Replace the default contract with this contract
3. Test locally: `clarinet console`
4. Deploy to testnet/mainnet using Clarinet's deployment commands

## Security Considerations

- Only the contract owner can register disasters and organizations
- Only the contract owner can disburse funds
- All fund movements are recorded on-chain for transparency
- Donations are locked in the contract until properly disbursed
