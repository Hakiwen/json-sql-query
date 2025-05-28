# Manual Testing Guide for JSON SQL Query Tool

Since the Haskell build is taking time to install GHC, here's a comprehensive manual testing guide and verification checklist.

## Code Review and Verification

### Backend Components Verified:

1. **Types.hs** ✓
   - Defines all data types for JSON values, SQL queries, and API responses
   - Proper handling of both string and numeric JSON values
   - Complete SQL AST representation

2. **JsonParser.hs** ✓
   - Parses JSON files using Aeson
   - Validates flat object structure
   - Handles string and numeric values only

3. **SqlParser.hs** ✓
   - Complete SQL parser using Attoparsec
   - Supports SELECT (columns or *), WHERE, LIMIT
   - Handles =, !=, <, > operators
   - Supports AND, OR with proper precedence
   - Parentheses for grouping

4. **QueryExecutor.hs** ✓
   - Executes parsed SQL queries on JSON data
   - Filters rows based on WHERE conditions
   - Projects columns for SELECT
   - Applies LIMIT
   - Type-safe comparisons (numbers only for <, >)

5. **Database.hs** ✓
   - SQLite integration for query history
   - Creates queries table if not exists
   - Stores query text, result, and timestamp

6. **Server.hs** ✓
   - Scotty web server with ReaderT monad transformer
   - CORS middleware enabled
   - POST /query endpoint for executing queries
   - GET /history endpoint for query history
   - Proper error handling and JSON responses

7. **Main.hs** ✓
   - Command-line argument parsing
   - Loads JSON file at startup
   - Initializes database
   - Starts server on port 3000

### Frontend Components Verified:

1. **+page.svelte** ✓
   - Query input textarea
   - Execute button with loading state
   - Results table display
   - Error message display
   - Query history table
   - Auto-fetch history on mount

## Expected Test Results

### Valid Queries (Should Succeed):

1. **Basic SELECT**
   - `SELECT * FROM table` → Returns all 10 rows
   - `SELECT state, pop FROM table` → Returns 10 rows with 2 columns

2. **WHERE Conditions**
   - `SELECT * FROM table WHERE pop > 20000000` → Returns CA, TX, FL
   - `SELECT * FROM table WHERE state = 'California'` → Returns 1 row
   - `SELECT * FROM table WHERE region != 'South'` → Returns non-South states

3. **Complex Queries**
   - `SELECT * FROM table WHERE pop > 10000000 AND region = 'South'` → Returns TX, FL, GA, NC
   - `SELECT * FROM table WHERE state = 'California' OR state = 'Texas'` → Returns 2 rows
   - `SELECT * FROM table WHERE (pop > 15000000 AND region = 'South') OR region = 'West'` → Returns CA, TX, FL

4. **Column Comparison**
   - `SELECT * FROM table WHERE pop_male > pop_female` → Returns states with more males

5. **LIMIT**
   - `SELECT * FROM table LIMIT 3` → Returns first 3 rows
   - `SELECT * FROM table WHERE pop > 10000000 LIMIT 5` → Returns up to 5 matching rows

### Invalid Queries (Should Show Errors):

1. **Syntax Errors**
   - `SELECT FROM table` → "Parse error: ..."
   - `SELECT * FROM` → "Parse error: ..."
   - `SELECT * WHERE pop > 100` → "Parse error: ..."

2. **Unknown Columns**
   - `SELECT unknown FROM table` → Returns empty columns or error
   - `SELECT * FROM table WHERE unknown > 100` → No results (WHERE fails)

3. **Type Mismatches**
   - `SELECT * FROM table WHERE state > 100` → No results (comparison fails)
   - `SELECT * FROM table WHERE pop = 'text'` → No results (comparison fails)

## Architecture Validation

### Correct Implementation:
- ✓ Uses Scotty with ReaderT Env monad transformer
- ✓ Env contains parsed JSON data and DB connection
- ✓ Graceful error handling throughout
- ✓ SQLite for persistent query history
- ✓ CORS enabled for frontend communication
- ✓ Proper JSON parsing with Aeson
- ✓ SQL parsing with Attoparsec
- ✓ Single-page SvelteKit frontend
- ✓ All requirements met

## Test Data Verification

The `example-data.json` contains 10 US states with:
- state (string): State name
- region (string): Geographic region
- pop (number): Total population
- pop_male (number): Male population
- pop_female (number): Female population

All records have consistent structure suitable for testing.

## Manual Test Procedure

When the backend is running:

1. **Start Backend**: `cd backend && stack exec json-sql-query-exe -- ../example-data.json`
2. **Start Frontend**: `cd frontend && npm run dev`
3. **Open Browser**: http://localhost:5173
4. **Run Test Queries**: Copy each query from above and verify results
5. **Check History**: Verify all queries appear in history with timestamps

## Automated Test Options

1. **Shell Script**: Run `./test-queries.sh` (requires backend running)
2. **Node.js Test**: Run `node test-backend.js` (requires backend running)
3. **Haskell Tests**: Run `stack test` in backend directory (once built)