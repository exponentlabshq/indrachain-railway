// IndraChain 3-Node Consensus Simulation
// Demonstrates UMPF-based blockchain consensus without deployment complexity

const crypto = require('crypto');
const EventEmitter = require('events');

class UMPFBlockchainSimulation extends EventEmitter {
    constructor() {
        super();
        this.nodes = new Map();
        this.networkDelay = 100; // Simulate network latency
        this.startTime = Date.now();
        
        // Initialize 3 validator nodes
        for (let i = 1; i <= 3; i++) {
            this.nodes.set(`node-${i}`, new ValidatorNode(`node-${i}`, this));
        }
        
        this.initializeGenesis();
    }
    
    // Initialize genesis block across all nodes
    initializeGenesis() {
        const genesisBlock = {
            index: 0,
            timestamp: this.startTime,
            transactions: [],
            previousHash: '0',
            hash: this.calculateHash({
                index: 0,
                timestamp: this.startTime,
                transactions: [],
                previousHash: '0'
            }),
            validator: 'genesis'
        };
        
        this.nodes.forEach(node => {
            node.blockchain = [genesisBlock];
            node.currentIndex = 0;
        });
        
        console.log('🎬 Genesis block initialized across all nodes');
        this.emit('genesis', genesisBlock);
    }
    
    // Calculate block hash (real cryptographic hashing)
    calculateHash(blockData) {
        const data = JSON.stringify(blockData);
        return crypto.createHash('sha256').update(data).digest('hex');
    }
    
    // Simulate network message passing between nodes
    async sendMessage(fromNodeId, toNodeId, message) {
        return new Promise(resolve => {
            setTimeout(() => {
                const targetNode = this.nodes.get(toNodeId);
                if (targetNode) {
                    targetNode.receiveMessage(fromNodeId, message);
                }
                resolve();
            }, this.networkDelay + Math.random() * 50); // Variable network delay
        });
    }
    
    // Broadcast message to all other nodes
    async broadcastMessage(fromNodeId, message) {
        const promises = [];
        this.nodes.forEach((node, nodeId) => {
            if (nodeId !== fromNodeId) {
                promises.push(this.sendMessage(fromNodeId, nodeId, message));
            }
        });
        return Promise.all(promises);
    }
    
    // Submit transaction to random node (entry point)
    async submitTransaction(from, to, amount) {
        const nodeIds = Array.from(this.nodes.keys());
        const randomNode = nodeIds[Math.floor(Math.random() * nodeIds.length)];
        const node = this.nodes.get(randomNode);
        
        const transaction = {
            id: crypto.randomUUID(),
            from,
            to,
            amount,
            timestamp: Date.now()
        };
        
        console.log(`💰 Transaction submitted to ${randomNode}:`, transaction);
        await node.receiveTransaction(transaction);
        return transaction.id;
    }
    
    // Get network status from all nodes
    getNetworkStatus() {
        const status = {
            nodes: {},
            consensus: true,
            blockHeight: 0,
            totalTransactions: 0
        };
        
        this.nodes.forEach((node, nodeId) => {
            const nodeStatus = node.getStatus();
            status.nodes[nodeId] = nodeStatus;
            
            // Check consensus (all nodes should have same block height)
            if (status.blockHeight === 0) {
                status.blockHeight = nodeStatus.blockHeight;
            } else if (status.blockHeight !== nodeStatus.blockHeight) {
                status.consensus = false;
            }
            
            status.totalTransactions += nodeStatus.pendingTransactions;
        });
        
        return status;
    }
    
    // Start consensus rounds
    startConsensus() {
        setInterval(() => {
            this.triggerConsensusRound();
        }, 20000); // 20-second block time
        
        console.log('🚀 Consensus mechanism started (20s rounds)');
    }
    
    // Trigger a consensus round
    async triggerConsensusRound() {
        const leader = this.selectLeader();
        console.log(`👑 ${leader.nodeId} selected as round leader`);
        
        await leader.proposeBlock();
        this.emit('consensus-round', {
            leader: leader.nodeId,
            timestamp: Date.now()
        });
    }
    
    // Simple leader selection (round-robin)
    selectLeader() {
        const nodeIds = Array.from(this.nodes.keys());
        const currentRound = Math.floor((Date.now() - this.startTime) / 20000);
        const leaderIndex = currentRound % nodeIds.length;
        return this.nodes.get(nodeIds[leaderIndex]);
    }
}

class ValidatorNode {
    constructor(nodeId, network) {
        this.nodeId = nodeId;
        this.network = network;
        this.blockchain = [];
        this.pendingTransactions = [];
        this.currentIndex = 0;
        this.messageQueue = [];
    }
    
    // Receive transaction from network
    async receiveTransaction(transaction) {
        this.pendingTransactions.push(transaction);
        console.log(`📨 ${this.nodeId} received transaction: ${transaction.id}`);
        
        // Broadcast to other nodes
        await this.network.broadcastMessage(this.nodeId, {
            type: 'transaction',
            payload: transaction
        });
    }
    
    // Receive message from another node
    receiveMessage(fromNodeId, message) {
        console.log(`📡 ${this.nodeId} received ${message.type} from ${fromNodeId}`);
        
        switch (message.type) {
            case 'transaction':
                if (!this.pendingTransactions.find(tx => tx.id === message.payload.id)) {
                    this.pendingTransactions.push(message.payload);
                }
                break;
                
            case 'block-proposal':
                this.validateProposedBlock(fromNodeId, message.payload);
                break;
                
            case 'block-vote':
                this.processBlockVote(fromNodeId, message.payload);
                break;
        }
    }
    
    // Propose new block (when selected as leader)
    async proposeBlock() {
        if (this.pendingTransactions.length === 0) {
            console.log(`⏭️  ${this.nodeId} has no transactions to include`);
            return;
        }
        
        const newBlock = {
            index: this.currentIndex + 1,
            timestamp: Date.now(),
            transactions: [...this.pendingTransactions], // Include all pending
            previousHash: this.blockchain[this.blockchain.length - 1].hash,
            validator: this.nodeId
        };
        
        newBlock.hash = this.network.calculateHash(newBlock);
        
        console.log(`📦 ${this.nodeId} proposing block #${newBlock.index} with ${newBlock.transactions.length} transactions`);
        
        // Broadcast block proposal
        await this.network.broadcastMessage(this.nodeId, {
            type: 'block-proposal',
            payload: newBlock
        });
        
        // Vote for own block
        this.voteForBlock(newBlock, true);
    }
    
    // Validate proposed block from another node
    async validateProposedBlock(proposerNodeId, proposedBlock) {
        // Simple validation: check index, previous hash, transaction validity
        const isValid = 
            proposedBlock.index === this.currentIndex + 1 &&
            proposedBlock.previousHash === this.blockchain[this.blockchain.length - 1].hash &&
            proposedBlock.transactions.length > 0;
        
        console.log(`✅ ${this.nodeId} validates ${proposerNodeId}'s block: ${isValid ? 'VALID' : 'INVALID'}`);
        
        // Send vote
        await this.network.broadcastMessage(this.nodeId, {
            type: 'block-vote',
            payload: {
                blockHash: proposedBlock.hash,
                vote: isValid,
                voter: this.nodeId
            }
        });
        
        if (isValid) {
            this.voteForBlock(proposedBlock, true);
        }
    }
    
    // Vote for a block
    voteForBlock(block, vote) {
        if (!this.blockVotes) this.blockVotes = new Map();
        
        if (!this.blockVotes.has(block.hash)) {
            this.blockVotes.set(block.hash, {
                block: block,
                votes: []
            });
        }
        
        const blockVote = this.blockVotes.get(block.hash);
        blockVote.votes.push({ voter: this.nodeId, vote });
        
        // Check if we have consensus (2/3 majority)
        const positiveVotes = blockVote.votes.filter(v => v.vote).length;
        if (positiveVotes >= 2) { // 2 out of 3 nodes
            this.finalizeBlock(block);
        }
    }
    
    // Process vote from another node
    processBlockVote(voterNodeId, voteMessage) {
        if (!this.blockVotes) this.blockVotes = new Map();
        
        if (this.blockVotes.has(voteMessage.blockHash)) {
            const blockVote = this.blockVotes.get(voteMessage.blockHash);
            blockVote.votes.push({
                voter: voterNodeId,
                vote: voteMessage.vote
            });
            
            // Check for consensus
            const positiveVotes = blockVote.votes.filter(v => v.vote).length;
            if (positiveVotes >= 2) {
                this.finalizeBlock(blockVote.block);
            }
        }
    }
    
    // Finalize block after consensus
    finalizeBlock(block) {
        // Avoid double-finalization
        if (this.currentIndex >= block.index) return;
        
        this.blockchain.push(block);
        this.currentIndex = block.index;
        
        // Remove processed transactions
        this.pendingTransactions = this.pendingTransactions.filter(tx => 
            !block.transactions.find(blockTx => blockTx.id === tx.id)
        );
        
        console.log(`🎉 ${this.nodeId} finalized block #${block.index} - Chain length: ${this.blockchain.length}`);
        
        this.network.emit('block-finalized', {
            nodeId: this.nodeId,
            block: block,
            chainLength: this.blockchain.length
        });
        
        // Clear votes for this round
        this.blockVotes = new Map();
    }
    
    // Get node status
    getStatus() {
        return {
            nodeId: this.nodeId,
            blockHeight: this.blockchain.length - 1, // Exclude genesis
            chainLength: this.blockchain.length,
            pendingTransactions: this.pendingTransactions.length,
            lastBlock: this.blockchain[this.blockchain.length - 1],
            syncStatus: 'synced' // Simplified for demo
        };
    }
}

// Export simulation class
module.exports = UMPFBlockchainSimulation;

// Demo usage if run directly
if (require.main === module) {
    console.log('🚀 Starting IndraChain 3-Node Consensus Simulation...\n');
    
    const simulation = new UMPFBlockchainSimulation();
    
    // Event listeners for observability
    simulation.on('genesis', (block) => {
        console.log('📍 Genesis block created:', block.hash.substring(0, 8));
    });
    
    simulation.on('consensus-round', (data) => {
        console.log(`\n🔄 Consensus Round - Leader: ${data.leader}`);
        console.log('Network Status:', simulation.getNetworkStatus());
    });
    
    simulation.on('block-finalized', (data) => {
        console.log(`✨ Block finalized by ${data.nodeId}: #${data.block.index}`);
    });
    
    // Start consensus
    simulation.startConsensus();
    
    // Simulate transactions
    setInterval(() => {
        const from = `user-${Math.floor(Math.random() * 100)}`;
        const to = `user-${Math.floor(Math.random() * 100)}`;
        const amount = Math.floor(Math.random() * 1000);
        
        simulation.submitTransaction(from, to, amount);
    }, 5000); // New transaction every 5 seconds
    
    // Status reports
    setInterval(() => {
        console.log('\n📊 Network Status:', simulation.getNetworkStatus());
    }, 30000); // Status every 30 seconds
}