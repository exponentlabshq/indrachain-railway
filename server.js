const http = require('http');
const UMPFBlockchainSimulation = require('./blockchain-simulation');

const PORT = process.env.PORT || 9933;
const NODE_ID = process.env.NODE_ID || 'indra-node-1';

// Initialize blockchain simulation
console.log('🚀 Initializing IndraChain 3-Node Consensus Simulation...');
const blockchain = new UMPFBlockchainSimulation();

// Event listeners for real-time updates
blockchain.on('block-finalized', (data) => {
  console.log(`✨ Block #${data.block.index} finalized - ${data.block.transactions.length} transactions`);
});

blockchain.on('consensus-round', (data) => {
  console.log(`🔄 Consensus round - Leader: ${data.leader}`);
});

// Start consensus mechanism
blockchain.startConsensus();

// Simulate transaction activity
setInterval(() => {
  const from = `wallet-${Math.floor(Math.random() * 50)}`;
  const to = `wallet-${Math.floor(Math.random() * 50)}`;
  const amount = Math.floor(Math.random() * 1000) + 1;
  
  blockchain.submitTransaction(from, to, amount);
}, 7000); // Transaction every 7 seconds

const server = http.createServer((req, res) => {
  console.log(`${new Date().toISOString()} - ${req.method} ${req.url}`);
  
  // Set CORS headers for all responses
  const corsHeaders = {
    'Access-Control-Allow-Origin': '*',
    'Access-Control-Allow-Methods': 'GET, POST, PUT, DELETE, OPTIONS',
    'Access-Control-Allow-Headers': 'Content-Type, Authorization, Accept',
    'Access-Control-Max-Age': '86400' // 24 hours
  };

  // Handle preflight OPTIONS requests
  if (req.method === 'OPTIONS') {
    res.writeHead(200, corsHeaders);
    res.end();
    return;
  }
  
  if (req.url === '/health') {
    const networkStatus = blockchain.getNetworkStatus();
    res.writeHead(200, { 
      'Content-Type': 'application/json',
      ...corsHeaders
    });
    res.end(JSON.stringify({ 
      status: networkStatus.consensus ? 'healthy' : 'degraded',
      service: 'indrachain-simulation',
      nodeId: NODE_ID,
      timestamp: new Date().toISOString(),
      consensus: networkStatus.consensus,
      activeNodes: Object.keys(networkStatus.nodes).length
    }));
  } else if (req.url === '/api/status') {
    const networkStatus = blockchain.getNetworkStatus();
    res.writeHead(200, { 
      'Content-Type': 'application/json',
      ...corsHeaders
    });
    res.end(JSON.stringify({
      connectedPeers: Object.keys(networkStatus.nodes).length,
      blockHeight: networkStatus.blockHeight,
      syncStatus: networkStatus.consensus ? 'synced' : 'diverged',
      nodeId: NODE_ID,
      totalTransactions: networkStatus.totalTransactions,
      nodes: networkStatus.nodes
    }));
  } else if (req.url === '/metrics') {
    const networkStatus = blockchain.getNetworkStatus();
    let totalProcessedTransactions = 0;
    
    // Calculate total processed transactions across all nodes
    Object.values(networkStatus.nodes).forEach(node => {
      if (node.lastBlock && node.lastBlock.transactions) {
        totalProcessedTransactions += node.lastBlock.transactions.length;
      }
    });
    
    res.writeHead(200, { 
      'Content-Type': 'text/plain',
      ...corsHeaders
    });
    res.end(`# IndraChain Simulation Metrics
indra_blocks_produced_total ${networkStatus.blockHeight}
indra_transactions_total ${totalProcessedTransactions}
indra_connected_peers ${Object.keys(networkStatus.nodes).length}
indra_pending_transactions ${networkStatus.totalTransactions}
indra_consensus_health ${networkStatus.consensus ? 1 : 0}
`);
  } else {
    res.writeHead(200, { 
      'Content-Type': 'application/json',
      ...corsHeaders
    });
    res.end(JSON.stringify({ 
      message: 'IndraChain Node Running on Railway',
      nodeId: NODE_ID,
      endpoints: ['/health', '/api/status', '/metrics']
    }));
  }
});

server.listen(PORT, '0.0.0.0', () => {
  console.log(`IndraChain Node ${NODE_ID} running on Railway`);
  console.log(`Server listening on port ${PORT}`);
  console.log(`Health check: http://localhost:${PORT}/health`);
});