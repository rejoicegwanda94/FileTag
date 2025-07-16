# 📁 FileTag - Secure File Metadata Contract

## 🚀 Overview

FileTag is a Clarity smart contract that enables secure file metadata management on the Stacks blockchain. It provides cryptographic proof of ownership, timestamping, and access control for digital files without storing the actual file content on-chain.

## ✨ Features

- 🔐 **Secure File Registration** - Register files with cryptographic hash verification
- ⏰ **Immutable Timestamping** - Blockchain-based proof of file creation time  
- 👤 **Ownership Management** - Transfer file ownership between users
- 🔑 **Access Control** - Grant and revoke file access permissions
- 🌐 **Public/Private Files** - Control file visibility settings
- 🔍 **Verification Tools** - Verify ownership and file integrity

## 🛠️ Usage

### Register a New File

```clarity
(contract-call? .FileTag register-file 
  0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef
  "my-document.pdf"
  u1024000
  "Important legal document"
  false)
```

### Transfer File Ownership

```clarity
(contract-call? .FileTag transfer-ownership u1 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7)
```

### Grant File Access

```clarity
(contract-call? .FileTag grant-access u1 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7)
```

### Verify File Ownership

```clarity
(contract-call? .FileTag verify-ownership u1 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7)
```

## 📋 Contract Functions

### Public Functions

- `register-file` - Register a new file with metadata
- `transfer-ownership` - Transfer file ownership to another user
- `grant-access` - Grant file access to a specific user
- `revoke-access` - Remove file access from a user
- `update-file-visibility` - Change file public/private status

### Read-Only Functions

- `get-file-info` - Get complete file metadata
- `get-file-by-hash` - Find file by its hash
- `verify-ownership` - Verify if user owns a file
- `verify-file-hash` - Verify file integrity
- `has-access` - Check if user has file access
- `get-file-timestamp` - Get file registration timestamp

## 🔧 Development

### Prerequisites

- Clarinet CLI installed
- Stacks wallet for testing

### Setup

```bash
clarinet new filetag-project
cd filetag-project
```

Copy the contract code to `contracts/FileTag.clar`

### Testing

```bash
clarinet console
```

### Deployment

```bash
clarinet deploy --testnet
```

## 🎯 Use Cases

- 📄 **Document Authenticity** - Prove document creation and ownership
- 🎨 **Digital Art Protection** - Timestamp and protect creative works  
- 📊 **Data Integrity** - Verify file hasn't been tampered with
- 🏢 **Corporate Records** - Maintain immutable business document records
- 🎓 **Academic Papers** - Establish publication priority and authorship

## ⚠️ Important Notes

- Only file metadata is stored on-chain, not actual file content
- File hashes must be unique across the entire contract
- Ownership transfers are irreversible
- Public files can be accessed by anyone
- Private files require explicit access grants

## 🤝 Contributing

Feel free to submit issues and enhancement requests!

## 📜 License

MIT License - feel free to use in your projects!


