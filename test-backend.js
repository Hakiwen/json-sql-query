// Test script for JSON SQL Query Backend
// Run with: node test-backend.js

const BASE_URL = 'http://localhost:3000';

// Test queries
const testQueries = [
  {
    name: "Select all",
    query: "SELECT * FROM table",
    expectSuccess: true
  },
  {
    name: "Select specific columns",
    query: "SELECT state, pop FROM table",
    expectSuccess: true
  },
  {
    name: "WHERE number comparison",
    query: "SELECT * FROM table WHERE pop > 20000000",
    expectSuccess: true
  },
  {
    name: "WHERE string comparison",
    query: "SELECT * FROM table WHERE state = 'California'",
    expectSuccess: true
  },
  {
    name: "Complex query",
    query: "SELECT state, region FROM table WHERE (pop > 10000000 AND region = 'South') OR region = 'West' LIMIT 5",
    expectSuccess: true
  },
  {
    name: "Invalid SQL",
    query: "SELECT FROM table",
    expectSuccess: false
  },
  {
    name: "Unknown column",
    query: "SELECT unknown FROM table",
    expectSuccess: false
  }
];

async function testQuery(testCase) {
  try {
    const response = await fetch(`${BASE_URL}/query`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ query: testCase.query })
    });
    
    const data = await response.json();
    const success = data.results.Right ? true : false;
    
    console.log(`\nTest: ${testCase.name}`);
    console.log(`Query: ${testCase.query}`);
    console.log(`Expected: ${testCase.expectSuccess ? 'Success' : 'Error'}`);
    console.log(`Result: ${success ? 'Success' : 'Error'}`);
    
    if (success && data.results.Right) {
      console.log(`Rows returned: ${data.results.Right.length}`);
      if (data.results.Right.length > 0) {
        console.log('First row:', JSON.stringify(data.results.Right[0], null, 2));
      }
    } else if (!success && data.results.Left) {
      console.log(`Error message: ${data.results.Left}`);
    }
    
    const passed = success === testCase.expectSuccess;
    console.log(`Status: ${passed ? '✓ PASSED' : '✗ FAILED'}`);
    
    return passed;
  } catch (error) {
    console.log(`\nTest: ${testCase.name}`);
    console.log(`Error: ${error.message}`);
    console.log('Status: ✗ FAILED (Connection error)');
    return false;
  }
}

async function checkServer() {
  try {
    const response = await fetch(`${BASE_URL}/history`);
    return response.ok;
  } catch {
    return false;
  }
}

async function runTests() {
  console.log('=== JSON SQL Query Backend Test Suite ===\n');
  
  // Check if server is running
  console.log('Checking server availability...');
  const serverUp = await checkServer();
  
  if (!serverUp) {
    console.log('\nERROR: Server is not running on http://localhost:3000');
    console.log('Please start the backend server first with:');
    console.log('  cd backend && stack exec json-sql-query-exe -- ../example-data.json\n');
    process.exit(1);
  }
  
  console.log('Server is running!\n');
  
  // Run tests
  let passed = 0;
  let failed = 0;
  
  for (const testCase of testQueries) {
    const result = await testQuery(testCase);
    if (result) passed++;
    else failed++;
  }
  
  // Get history
  console.log('\n=== Checking Query History ===');
  try {
    const response = await fetch(`${BASE_URL}/history`);
    const history = await response.json();
    console.log(`Total queries in history: ${history.length}`);
    if (history.length > 0) {
      console.log('Latest query:', {
        query: history[0].queryText,
        result: history[0].result,
        timestamp: history[0].timestamp
      });
    }
  } catch (error) {
    console.log('Failed to fetch history:', error.message);
  }
  
  // Summary
  console.log('\n=== Test Summary ===');
  console.log(`Total tests: ${testQueries.length}`);
  console.log(`Passed: ${passed}`);
  console.log(`Failed: ${failed}`);
  console.log(`Success rate: ${(passed / testQueries.length * 100).toFixed(1)}%`);
}

// Run the tests
runTests();