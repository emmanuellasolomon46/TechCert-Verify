# TechCert Verify

A decentralized IT certification verification platform built on Stacks blockchain using Clarity smart contracts.

## Overview

TechCert Verify provides a secure and transparent way to issue, verify and manage technical certifications. The platform enables certification authorities to issue verifiable credentials, professionals to showcase their skills, and employers to validate certifications.

## Features

- **Certification Management**
  - Issue new certifications with unique IDs
  - Track certification validity and expiry
  - Update certification status
  - Verify certification authenticity

- **Skills & Endorsements** 
  - User skill profile management
  - Peer endorsements for certifications
  - Track professional development

## Smart Contract Functions

### Certification Functions

```clarity
issue-certification(cert-id, holder, cert-name, expiry-date)
verify-certification(cert-id) 
get-certification(cert-id)
update-cert-status(cert-id, new-status)
```

### Skills Management

```clarity
add-skill(skill)
get-user-skills(user)
```

### Endorsements

```clarity
endorse-certification(cert-id)
get-cert-endorsements(cert-id)
```

## Data Structure

The contract maintains the following data maps:

- `certifications`: Stores certification details
- `user-certifications`: Maps users to their certifications
- `user-skills`: Tracks user skill profiles  
- `cert-endorsements`: Records certification endorsements

## Error Codes

- `100`: Not authorized
- `101`: Certificate already exists
- `102`: Certificate not found

## Development

This project uses:
- Clarity smart contract language
- Clarinet for development and testing
- Vitest for test automation

### Setup

1. Install Clarinet
2. Clone the repository
3. Run tests:
```bash
clarinet test
```

## Security

- Only contract owner can issue/update certifications
- Built-in verification checks
- Immutable certification records

## License

MIT

## Contributing

Contributions welcome! Please read the contributing guidelines before submitting PRs.
