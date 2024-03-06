document.addEventListener("DOMContentLoaded", function() {
    // Fetch the JSON data from the Flask server
    fetch('/raw')
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
            headerCell.colSpan = "4";
            headerCell.innerHTML = fileName.concat(" -- ", fileDetails.File_State);

            // Add file content to the table
            fileDetails.File_Content.forEach(lineDetail => {
                const row = table.insertRow();

                if (lineDetail.Line_Status === "+") {
                    row.className = 'green-highlight'; // Use a class to style the row
                } else if (lineDetail.Line_Status === "-") {
                    row.className = 'pink-highlight'; // Use a class to style the row
                }

                const cellOldLine = row.insertCell();
                cellOldLine.textContent = lineDetail.Line_Old;

                const cellNewLine = row.insertCell();
                cellNewLine.textContent = lineDetail.Line_New;

                const cellChange = row.insertCell();
                cellChange.textContent = lineDetail.Line_Status;

                const cellValue = row.insertCell();
                cellValue.textContent = lineDetail.Line_Value;

            });

            // Append the table to the result container
            resultContainer.appendChild(table);
            resultContainer.appendChild(document.createElement('br')); // Add a spacer after each table
        });
    })
    .catch(error => console.error('Error fetching data:', error));
});
