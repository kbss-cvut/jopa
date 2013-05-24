if (!window["spinnerIncrement"]) {
    var spinnerIncrement = {};
}
function init(componentID, increment) {
    spinnerIncrement[componentID] = Number(increment);
    if (isNaN(spinnerIncrement[componentID]) || spinnerIncrement[componentID] == 0 ) {
        spinnerIncrement[componentID]= 1;
    }
}

function changeNumber(componentID, amount) {
    var entry = document.getElementById(componentID+":"+"number");
    var val = Number(entry.value);
    if (isNaN(val)) { val = 0; }
    entry.value = val + (amount * spinnerIncrement[componentID]);
    return false;
}