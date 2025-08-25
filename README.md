# IndraChain Railway Deployment

[![Deploy on Railway](https://railway.app/button.svg)](https://railway.app/template/indrachain)

## Overview

This repository contains the complete Infrastructure as a Service (IaaS) deployment configuration for IndraChain, a Haskell-based blockchain with native Account Abstraction, optimized for Railway.com deployment.

IndraChain leverages the Universal Monad Patterns Framework (UMPF) to provide:
- **Native Account Abstraction** - Custom transaction validation rules
- **Hybrid Ledger Model** - EUTXO + Account-based transactions
- **Formal Verification** - Haskell type safety with Liquid Haskell
- **Pattern Recognition** - UMPF cross-domain equivalency analysis

## Quick Deploy to Railway

### Option 1: One-Click Deploy (Recommended)
1. Click the "Deploy on Railway" button above
2. Connect your GitHub account
3. Railway will automatically deploy all services
4. Monitor deployment progress in Railway dashboard

### Option 2: Manual GitHub Deploy
1. Fork this repository
2. Go to [railway.app](https://railway.app)
3. Create New Project → "Deploy from GitHub repo"
4. Select your forked repository
5. Railway detects configuration automatically

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                 IndraChain Railway Architecture              │
├─────────────────────────────────────────────────────────────┤
│  Layer 4: Orchestration (Free Monad)                       │
│  ┌─────────────────┐ ┌─────────────────┐ ┌─────────────────┐│
│  │  DeFi Protocols │ │   Governance    │ │  Cross-Chain    ││
│  │    Service      │ │    Service      │ │    Bridges      ││
│  └─────────────────┘ └─────────────────┘ └─────────────────┘│
├─────────────────────────────────────────────────────────────┤
│  Layer 3: Interaction (IO/STM/Async)                       │
│  ┌─────────────────┐ ┌─────────────────┐ ┌─────────────────┐│
│  │   API Gateway   │ │  Oracle Service │ │  AA Processor   ││
│  │  (Rate Limiting)│ │  (External Data)│ │ (Custom Rules)  ││
│  └─────────────────┘ └─────────────────┘ └─────────────────┘│
├─────────────────────────────────────────────────────────────┤
│  Layer 2: Evolution (State/Reader/Writer)                  │
│  ┌─────────────────┐ ┌─────────────────┐ ┌─────────────────┐│
│  │ IndraCore VM 1  │ │ IndraCore VM 2  │ │ IndraCore VM 3  ││
│  │ (Validator Node)│ │ (Validator Node)│ │ (Validator Node)││
│  └─────────────────┘ └─────────────────┘ └─────────────────┘│
├─────────────────────────────────────────────────────────────┤
│  Layer 1: Uncertainty (Maybe/Either/List)                  │
│  ┌─────────────────┐ ┌─────────────────┐ ┌─────────────────┐│
│  │   PostgreSQL    │ │     Redis       │ │   Monitoring    ││
│  │ (Ledger State)  │ │   (Cache)       │ │   (Metrics)     ││
│  └─────────────────┘ └─────────────────┘ └─────────────────┘│
└─────────────────────────────────────────────────────────────┘
```

## Services

### Core Blockchain Services
- **IndraCore VM** (3 nodes) - Haskell-based validator nodes with IndraPoS consensus
- **AA Processor** - Account Abstraction transaction processor
- **API Gateway** - Rate-limited REST API with CORS support

### Infrastructure Services
- **PostgreSQL** - Ledger state storage with automated backups
- **Redis** - Transaction pool and state caching
- **Monitoring** - Prometheus + Grafana with custom UMPF metrics

### Analytics Services
- **UMPF Analyzer** - Pattern recognition and cross-domain equivalency analysis

## Deployment Environments

| Environment | Cost/Month | Nodes | Use Case |
|-------------|-----------|-------|----------|
| **Development** | ~$30 | 1 | Local testing, prototyping |
| **Staging** | ~$90 | 3 | Testnet, community testing |
| **Production** | ~$200 | 5+ | Mainnet, enterprise deployment |

## Environment Variables

Railway automatically configures most variables, but you can customize:

### IndraCore VM
- `NODE_ID` - Unique validator identifier
- `CONSENSUS_ROLE` - validator, full-node, or archive
- `NETWORK_PORT` - P2P networking port (default: 9944)
- `RPC_PORT` - JSON-RPC API port (default: 9933)

### Account Abstraction Processor  
- `MAX_AA_COMPLEXITY` - Maximum complexity for AA rules (default: 1000)
- `COLLATERAL_THRESHOLD` - Minimum collateral for AA transactions (default: 10000)
- `PROCESSING_THREADS` - Number of processing threads (default: 4)

### UMPF Analyzer
- `ANALYSIS_INTERVAL` - Pattern analysis frequency in seconds (default: 60)
- `EQUIVALENCY_THRESHOLD` - Minimum equivalency match score (default: 0.85)
- `PATTERN_DISCOVERY_ENABLED` - Enable automatic pattern discovery (default: true)

## Local Development

### Prerequisites
- Docker and Docker Compose
- Haskell (GHC 9.4+) for local development
- Node.js (for Railway CLI)

### Run Locally
```bash
# Clone repository
git clone https://github.com/YOUR_USERNAME/indrachain-railway.git
cd indrachain-railway

# Start all services
docker-compose up

# Access services
# API Gateway: http://localhost:3000
# Grafana Monitoring: http://localhost:3001 (admin/admin)
# Prometheus: http://localhost:9090
# UMPF Analyzer: http://localhost:5000
```

### Build Individual Services
```bash
# Build IndraCore VM
cd services/indracore-vm
cabal build

# Build AA Processor
cd services/aa-processor  
cabal build

# Run tests
cabal test
```

## Monitoring and Observability

### Grafana Dashboards
- **IndraChain Overview** - Network status, block production, transaction metrics
- **UMPF Pattern Analysis** - Pattern discovery rates, equivalency matching
- **Account Abstraction** - AA transaction processing, validation failures
- **System Resources** - CPU, memory, disk usage across all nodes

### Prometheus Metrics
- `indra_blocks_produced_total` - Total blocks produced by validator
- `indra_transactions_total` - Transaction processing metrics
- `indra_aa_queue_depth` - Account Abstraction transaction queue
- `umpf_patterns_discovered_total` - UMPF pattern discovery rate
- `umpf_equivalency_match_accuracy` - Pattern matching accuracy

### Health Checks
All services include built-in health check endpoints:
- IndraCore VM: `http://node:9933/health`
- AA Processor: `http://aa-processor:8080/health`
- API Gateway: `http://api-gateway:3000/health`
- UMPF Analyzer: `http://umpf-analyzer:5000/health`

## API Documentation

### IndraCore RPC API
```bash
# Get node status
curl http://your-railway-app.railway.app/api/status

# Submit transaction
curl -X POST http://your-railway-app.railway.app/api/transaction \
  -H "Content-Type: application/json" \
  -d '{"from":"0x...","to":"0x...","value":"1000"}'

# Query account abstraction rules
curl http://your-railway-app.railway.app/api/aa/rules/0x...
```

### UMPF Analysis API
```bash
# Discover patterns
curl http://your-railway-app.railway.app/umpf/patterns

# Check equivalency  
curl -X POST http://your-railway-app.railway.app/umpf/equivalency \
  -H "Content-Type: application/json" \
  -d '{"domain1":"blockchain","domain2":"database","threshold":0.8}'
```

## Security

### Railway Security Features
- Automatic TLS/HTTPS for all public endpoints
- Private networking between services
- Encrypted environment variable storage
- Regular security updates and patches

### IndraChain Security
- Formal verification with Liquid Haskell
- Type system prevents common vulnerabilities
- Account Abstraction with collateral requirements
- Byzantine fault tolerance with 3+ validators

## Troubleshooting

### Common Issues

**Services Not Starting**
- Check Railway service logs in dashboard
- Verify environment variables are set correctly
- Ensure Docker builds complete successfully

**Database Connection Issues**
- Verify PostgreSQL service is running
- Check DATABASE_URL environment variable
- Review connection pool settings

**High Memory Usage**
- Monitor Grafana resource dashboard
- Adjust GHC RTS options in Dockerfile
- Scale services horizontally if needed

### Support
- Railway Documentation: https://docs.railway.app
- IndraChain Issues: https://github.com/YOUR_USERNAME/indrachain-railway/issues
- Railway Community: https://railway.app/discord

## Contributing

1. Fork the repository
2. Create feature branch: `git checkout -b feature/amazing-feature`
3. Make changes and test locally with `docker-compose up`
4. Commit changes: `git commit -m 'Add amazing feature'`
5. Push to branch: `git push origin feature/amazing-feature`
6. Open Pull Request

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Roadmap

### Phase 1: Core Development (Current)
- ✅ Railway deployment configuration
- ✅ Multi-service Docker architecture
- ✅ UMPF pattern recognition integration
- ✅ Account Abstraction processor
- ✅ Monitoring and alerting

### Phase 2: Advanced Features (Next 3-6 months)
- [ ] Cross-chain bridge implementations
- [ ] Advanced DeFi protocol templates
- [ ] Mobile-friendly AA wallet interface
- [ ] Formal verification test suite
- [ ] Performance optimization

### Phase 3: Ecosystem Growth (6-12 months)
- [ ] Developer SDK and documentation
- [ ] Community governance implementation  
- [ ] Enterprise partnership integrations
- [ ] Multi-region deployment templates
- [ ] Advanced UMPF training models

---

## Quick Links

- **Railway Dashboard**: [Access your deployed services](https://railway.app/dashboard)
- **IndraChain Whitepaper**: [Technical specifications](../indrachain-whitepaper.md)
- **UMPF Framework**: [Universal Monad Patterns documentation](../the-rosetta-stone-thewayofgoodvibes-deathnote-complete.md)
- **Railway Pricing**: [Transparent pricing calculator](https://railway.app/pricing)

---

*IndraChain: Bridging Ancient Wisdom with Modern Technology through Mathematical Excellence*

**Deployed with ❤️ on Railway**