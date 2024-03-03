document.addEventListener("DOMContentLoaded", function() {
    // Fetch the JSON data from the Flask server
    fetch('/example')
    .then(response => response.json())
    .then(data => {
        const resultContainer = document.getElementById("result");

        // Clear the container
        resultContainer.innerHTML = "";

        // Process each file's details
        Object.entries(data).forEach(([fileName, fileDetails]) => {
            // Create a table for each file
            const table = document.createElement('table');
            const headerRow = table.insertRow();
            const headerCell = headerRow.insertCell();
            headerCell.colSpan = "2";
            headerCell.innerHTML = fileName;

            // Add file content to the table
            fileDetails.File_Content.forEach(lineDetail => {
                const row = table.insertRow();
                const cell = row.insertCell();

                // Apply styling based on the line status
                let line = lineDetail.Line_Value;
                if (lineDetail.Line_Status === "added") {
                    cell.innerHTML = `<mark class='green'>${line}</mark>`;
                } else if (lineDetail.Line_Status === "removed") {
                    cell.innerHTML = `<mark class='pink'>${line}</mark>`;
                } else {
                    cell.textContent = line;
                }
            });

            // Append the table to the result container
            resultContainer.appendChild(table);
            resultContainer.appendChild(document.createElement('br')); // Add a spacer after each table
        });
    })
    .catch(error => console.error('Error fetching data:', error));
});
