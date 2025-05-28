# JSON SQL Query Tool

A web application that enables SQL querying of JSON data using Haskell backend and SvelteKit frontend.

## Features

- Execute SQL queries on flat JSON array data
- Support for SELECT (columns or *), WHERE, and LIMIT clauses
- Binary conditions: =, !=, <, > (comparison operators work on numbers only)
- Logical operators: AND, OR with parentheses support
- Web interface with real-time query execution
- Query history stored in SQLite database
- Graceful error handling for invalid queries

## Prerequisites

- [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)
- [Node.js](https://nodejs.org/) (v16 or higher) and npm
- SQLite3 (usually pre-installed on most systems)

## Quick Start

### 1. Clone the repository
```bash
git clone <repository-url>
cd json-sql-query
```

### 2. Setup and run the backend

```bash
# Navigate to backend directory
cd backend

# Build the Haskell project (this will download dependencies on first run)
stack build

# Run the server with the example data
stack exec json-sql-query-exe -- ../example-data.json

# Or use your own JSON file
stack exec json-sql-query-exe -- /path/to/your/data.json

# Optionally specify a database file location
stack exec json-sql-query-exe -- ../example-data.json ./mydb.db
```

The backend server will start on http://localhost:3000

### 3. Setup and run the frontend

In a new terminal:

```bash
# Navigate to frontend directory
cd frontend

# Install dependencies
npm install

# Run the development server
npm run dev
```

The frontend will be available at http://localhost:5173

## Usage

1. Open http://localhost:5173 in your browser
2. Enter SQL queries in the text area
3. Click "Execute Query" to run the query
4. View results in the table below
5. See query history at the bottom of the page

## JSON Data Format

Your JSON file must be an array of objects with consistent keys:

```json
[
  {"state": "California", "region": "West", "pop": 39538223, "pop_male": 19453834, "pop_female": 20084389},
  {"state": "Texas", "region": "South", "pop": 29145505, "pop_male": 14508122, "pop_female": 14637383}
]
```

Requirements:
- Top-level must be an array
- Each object must have the same keys
- Values can only be strings or numbers
- No nested objects, arrays, or null values

## SQL Query Examples

### Basic SELECT queries
```sql
-- Select all columns
SELECT * FROM table

-- Select specific columns
SELECT state, pop FROM table

-- With LIMIT
SELECT * FROM table LIMIT 5
```

### WHERE clause examples
```sql
-- Simple comparison
SELECT * FROM table WHERE pop > 10000000

-- String comparison
SELECT * FROM table WHERE state = 'California'

-- Not equal
SELECT * FROM table WHERE region != 'West'

-- Multiple conditions with AND
SELECT * FROM table WHERE pop > 1000000 AND region = 'South'

-- OR conditions
SELECT * FROM table WHERE state = 'California' OR state = 'Texas'

-- Complex conditions with parentheses
SELECT * FROM table WHERE (pop > 20000000 AND region = 'South') OR state = 'California'

-- Column comparisons
SELECT * FROM table WHERE pop_male > pop_female
```

## Testing the Application

### Manual Testing Steps

1. **Test basic functionality:**
   ```sql
   SELECT * FROM table
   ```
   Should return all rows from your JSON data.

2. **Test column selection:**
   ```sql
   SELECT state, pop FROM table
   ```
   Should return only the specified columns.

3. **Test WHERE conditions:**
   ```sql
   SELECT * FROM table WHERE pop > 20000000
   ```
   Should filter rows based on population.

4. **Test complex queries:**
   ```sql
   SELECT state, region FROM table WHERE (pop > 10000000 AND region = 'South') OR region = 'West' LIMIT 5
   ```

5. **Test error handling:**
   - Invalid SQL: `SELECT FROM table`
   - Unknown column: `SELECT unknown_column FROM table`
   - Type mismatch: `SELECT * FROM table WHERE state > 100`

### Expected Behavior

- Valid queries should display results in a table
- Invalid queries should show error messages
- All queries (successful or failed) should appear in history
- The application should never crash

## Building for Production

### Backend
```bash
cd backend
stack build --copy-bins
# Binary will be in: $(stack path --local-install-root)/bin/
```

### Frontend
```bash
cd frontend
npm run build
# Static files will be in: build/
```

## Architecture Overview

### Backend (Haskell)
- **Web Framework**: Scotty with ReaderT monad transformer
- **JSON Parsing**: Aeson for parsing input JSON files
- **SQL Parsing**: Attoparsec for parsing SQL queries
- **Database**: SQLite for storing query history
- **Error Handling**: Graceful failures with descriptive messages

### Frontend (SvelteKit)
- Single-page application
- Client-side only (no server-side rendering)
- Fetch API for backend communication
- Reactive UI updates

## Troubleshooting

### Common Issues

1. **Port already in use:**
   - Backend runs on port 3000
   - Frontend runs on port 5173
   - Make sure these ports are free

2. **CORS errors:**
   - The backend includes CORS middleware
   - If issues persist, check browser console

3. **Build errors:**
   - Run `stack clean` and rebuild
   - Make sure you have the correct GHC version

4. **JSON parsing errors:**
   - Verify your JSON file is valid
   - Check that all objects have the same keys
   - Ensure no null values or nested structures

## Limitations

- Only supports SELECT queries (no INSERT, UPDATE, DELETE)
- No JOIN, GROUP BY, or aggregate functions
- Comparison operators (<, >) only work with numbers
- Column names cannot be SQL reserved words
- All table references must use the keyword "table"

## Repository Structure

```
json-sql-query/
├── backend/
│   ├── app/
│   │   └── Main.hs          # Entry point
│   ├── src/
│   │   ├── Types.hs         # Data type definitions
│   │   ├── JsonParser.hs    # JSON parsing logic
│   │   ├── SqlParser.hs     # SQL parsing with Attoparsec
│   │   ├── QueryExecutor.hs # Query execution engine
│   │   ├── Database.hs      # SQLite operations
│   │   └── Server.hs        # Scotty web server
│   ├── package.yaml         # Haskell dependencies
│   └── stack.yaml           # Stack configuration
├── frontend/
│   ├── src/
│   │   └── routes/
│   │       └── +page.svelte # Main UI component
│   ├── package.json         # Node dependencies
│   └── vite.config.js       # Vite configuration
├── example-data.json        # Sample data file
└── README.md               # This file
```