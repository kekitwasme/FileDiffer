document.addEventListener("DOMContentLoaded", function() {
    // Fetch the JSON data from the Flask server
    fetch('/test')
    .then(response => response.json())
    .then(data => {
        const resultContainer = document.getElementById("result");

        // Clear the container
        resultContainer.innerHTML = "";

        Object.entries(data).forEach((item) => {
            theItem = item[1]

            const table = document.createElement('table');
            const headerRow = table.insertRow();
            const headerCell = headerRow.insertCell();
            headerCell.colSpan = "4";

            if(theItem[0]=='M'){
                if(theItem[1] === theItem[2]){
                    headerCell.innerHTML = theItem[1];
                }
                else{
                    headerCell.innerHTML = theItem[1].concat(" --> "+theItem[2]);
                }

                theItem[3].forEach((theLine, index) => {

                    const row = table.insertRow();

                    if (theLine[0] === "+") {
                        row.className = 'green-highlight'; // Use a class to style the row
                    } else if (theLine[0] === "-") {
                        row.className = 'pink-highlight'; // Use a class to style the row
                    }

                    const num = row.insertCell();
                    num.textContent = index

                    const addLine = row.insertCell();
                    addLine.textContent = theLine.substring(2);
                })

            }
            else {
                headerCell.innerHTML = theItem[1];

                theItem[2].forEach((theLine, index) => {

                    const row = table.insertRow();

                    if (theItem[0] === "A") {
                        row.className = 'green-highlight'; // Use a class to style the row
                    } else if (theItem[0] === "D") {
                        row.className = 'pink-highlight'; // Use a class to style the row
                    }

                    const num = row.insertCell();
                    num.textContent = index

                    const addLine = row.insertCell();
                    addLine.textContent = theLine
                })
            }
            
            resultContainer.appendChild(table);
            resultContainer.appendChild(document.createElement('br')); // Add a spacer after each table
        });
    })
    .catch(error => console.error('Error fetching data:', error));
});