//just a little file to print out boards of different sizes for
//visualisation
size = parseInt(process.argv[2]);
odd = true;

for (var i = 0; i < size; i++) {
    for (var j = 1; j <= size; j++) {
        var num = i * size + j;
        if (num % 2 == 0) {} else {
            num = num / 2 + 0.5;
            if (num < 10) {
                num = " " + num;
            }
            if (odd) {
                process.stdout.write("|  ||" + (num) + "|");
            } else {
                process.stdout.write("|" + (num) + "||  |");
            }
        }
    }
    odd = !odd;
    console.log();
}
