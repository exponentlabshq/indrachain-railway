const http = require('http');

const PORT = process.env.PORT || 9933;
const NODE_ID = process.env.NODE_ID || 'indra-node-1';

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
    res.writeHead(200, { 
      'Content-Type': 'application/json',
      ...corsHeaders
    });
    res.end(JSON.stringify({ 
      status: 'healthy', 
      service: 'indrachain-node',
      nodeId: NODE_ID,
      timestamp: new Date().toISOString()
    }));
  } else if (req.url === '/api/status') {
    res.writeHead(200, { 
      'Content-Type': 'application/json',
      ...corsHeaders
    });
    res.end(JSON.stringify({
      connectedPeers: 3,
      blockHeight: Math.floor(Date.now() / 20000), // New block every 20 seconds
      syncStatus: 'synced',
      nodeId: NODE_ID
    }));
  } else if (req.url === '/metrics') {
    res.writeHead(200, { 
      'Content-Type': 'text/plain',
      ...corsHeaders
    });
    res.end(`# IndraChain Metrics
indra_blocks_produced_total ${Math.floor(Date.now() / 20000)}
indra_transactions_total ${Math.floor(Math.random() * 1000)}
indra_connected_peers 3
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