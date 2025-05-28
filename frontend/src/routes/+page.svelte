<script>
  let query = '';
  let loading = false;
  let error = '';
  let results = [];
  let history = [];

  async function executeQuery() {
    if (!query.trim()) return;
    
    loading = true;
    error = '';
    
    try {
      const response = await fetch('http://localhost:3000/query', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({ query })
      });
      
      const data = await response.json();
      
      if (data.results.Left) {
        error = data.results.Left;
        results = [];
      } else {
        error = '';
        results = data.results.Right || [];
      }
      
      await fetchHistory();
    } catch (err) {
      error = 'Failed to execute query: ' + err.message;
      results = [];
    } finally {
      loading = false;
    }
  }
  
  async function fetchHistory() {
    try {
      const response = await fetch('http://localhost:3000/history');
      history = await response.json();
    } catch (err) {
      console.error('Failed to fetch history:', err);
    }
  }
  
  // Fetch history on mount
  fetchHistory();
</script>

<div style="padding: 20px; max-width: 1200px; margin: 0 auto;">
  <h1>JSON SQL Query Tool</h1>
  
  <div style="margin-bottom: 20px;">
    <textarea 
      bind:value={query}
      placeholder="Enter SQL query (e.g., SELECT * FROM data WHERE pop > 1000000)"
      style="width: 100%; height: 100px; font-family: monospace;"
    ></textarea>
    <button 
      onclick={executeQuery} 
      disabled={loading}
      style="margin-top: 10px; padding: 10px 20px;"
    >
      {loading ? 'Executing...' : 'Execute Query'}
    </button>
  </div>
  
  {#if error}
    <div style="background: #fee; padding: 10px; margin-bottom: 20px; color: #c00;">
      Error: {error}
    </div>
  {/if}
  
  {#if results.length > 0}
    <div style="margin-bottom: 20px;">
      <h2>Results ({results.length} rows)</h2>
      <div style="overflow-x: auto;">
        <table style="border-collapse: collapse; width: 100%;">
          <thead>
            <tr>
              {#each Object.keys(results[0]) as column}
                <th style="border: 1px solid #ddd; padding: 8px; background: #f5f5f5;">
                  {column}
                </th>
              {/each}
            </tr>
          </thead>
          <tbody>
            {#each results as row}
              <tr>
                {#each Object.values(row) as value}
                  <td style="border: 1px solid #ddd; padding: 8px;">
                    {value}
                  </td>
                {/each}
              </tr>
            {/each}
          </tbody>
        </table>
      </div>
    </div>
  {/if}
  
  <div>
    <h2>Query History</h2>
    <div style="overflow-x: auto;">
      <table style="border-collapse: collapse; width: 100%;">
        <thead>
          <tr>
            <th style="border: 1px solid #ddd; padding: 8px; background: #f5f5f5;">Time</th>
            <th style="border: 1px solid #ddd; padding: 8px; background: #f5f5f5;">Query</th>
            <th style="border: 1px solid #ddd; padding: 8px; background: #f5f5f5;">Result</th>
          </tr>
        </thead>
        <tbody>
          {#each history as item}
            <tr>
              <td style="border: 1px solid #ddd; padding: 8px;">{item.timestamp}</td>
              <td style="border: 1px solid #ddd; padding: 8px; font-family: monospace;">{item.queryText}</td>
              <td style="border: 1px solid #ddd; padding: 8px;">{item.result}</td>
            </tr>
          {/each}
        </tbody>
      </table>
    </div>
  </div>
</div>
