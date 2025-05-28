#!/bin/bash

# Test script for JSON SQL Query Backend API
# This script tests various SQL queries against the backend

BASE_URL="http://localhost:3000"

echo "=== JSON SQL Query Backend Test Suite ==="
echo ""

# Function to test a query
test_query() {
    local query="$1"
    local description="$2"
    
    echo "Test: $description"
    echo "Query: $query"
    
    response=$(curl -s -X POST "$BASE_URL/query" \
        -H "Content-Type: application/json" \
        -d "{\"query\": \"$query\"}")
    
    echo "Response: $response"
    echo "---"
    echo ""
}

# Check if server is running
echo "Checking if server is running on $BASE_URL..."
if ! curl -s -f "$BASE_URL/history" > /dev/null; then
    echo "ERROR: Server is not running on $BASE_URL"
    echo "Please start the backend server first with:"
    echo "  cd backend && stack exec json-sql-query-exe -- ../example-data.json"
    exit 1
fi

echo "Server is running! Starting tests..."
echo ""

# Test 1: Basic SELECT all
test_query "SELECT * FROM table" "Select all columns and rows"

# Test 2: Select specific columns
test_query "SELECT state, pop FROM table" "Select specific columns"

# Test 3: SELECT with LIMIT
test_query "SELECT * FROM table LIMIT 3" "Select with LIMIT clause"

# Test 4: WHERE with number comparison
test_query "SELECT state, pop FROM table WHERE pop > 20000000" "WHERE clause with number comparison"

# Test 5: WHERE with string comparison
test_query "SELECT * FROM table WHERE state = 'California'" "WHERE clause with string equality"

# Test 6: WHERE with != operator
test_query "SELECT state, region FROM table WHERE region != 'South'" "WHERE clause with not equal"

# Test 7: WHERE with AND
test_query "SELECT state, pop FROM table WHERE pop > 10000000 AND region = 'South'" "WHERE clause with AND"

# Test 8: WHERE with OR
test_query "SELECT state FROM table WHERE state = 'California' OR state = 'Texas'" "WHERE clause with OR"

# Test 9: Complex WHERE with parentheses
test_query "SELECT state, region, pop FROM table WHERE (pop > 15000000 AND region = 'South') OR region = 'West'" "Complex WHERE with parentheses"

# Test 10: Column comparison
test_query "SELECT state, pop_male, pop_female FROM table WHERE pop_male > pop_female" "WHERE clause comparing columns"

# Test 11: Combined SELECT, WHERE, and LIMIT
test_query "SELECT state, pop FROM table WHERE pop > 10000000 LIMIT 5" "Combined SELECT, WHERE, and LIMIT"

# Error test cases
echo "=== Error Handling Tests ==="
echo ""

# Test E1: Invalid SQL syntax
test_query "SELECT FROM table" "Invalid SQL - missing columns"

# Test E2: Unknown column
test_query "SELECT unknown_column FROM table" "Unknown column name"

# Test E3: Type mismatch
test_query "SELECT * FROM table WHERE state > 100" "Type mismatch - comparing string to number"

# Test E4: Invalid operator position
test_query "SELECT * FROM table WHERE pop" "Incomplete WHERE clause"

# Test E5: Missing table name
test_query "SELECT * FROM WHERE pop > 1000" "Missing table name"

# Fetch and display history
echo "=== Query History ==="
echo ""
curl -s "$BASE_URL/history" | python3 -m json.tool 2>/dev/null || curl -s "$BASE_URL/history"
echo ""

echo "=== Test Suite Complete ==="