struct Cups {
    // [0] - which cup comes after Cup 1
    // [1] - which cup comes after Cup 2
    // [n-th] - which cup comes after Cup (n+1)
    links: Vec<usize>,
    selected: usize,
}

impl Cups {
    fn safe_decrement(&self, x: usize) -> usize {
        (if x == 0 { self.links.len() } else { x }) - 1
    }

    fn iteration(&mut self) {
        let current = self.selected;
        // The crab picks up the three cups that are immediately clockwise of the current cup.
        let a = self.links[current];
        let b = self.links[a];
        let c = self.links[b];

        self.links[current] = self.links[c];

        // The crab selects a destination cup: the cup with a label equal to the current cup's label
        // minus one.
        let mut destination = self.safe_decrement(current);

        // If this would select one of the cups that was just picked up, the crab will keep
        // subtracting one until it finds a cup that wasn't just picked up. If at any point in this
        // process the value goes below the lowest value on any cup's label, it wraps around to the
        // highest value on any cup's label instead.
        while destination == a || destination == b || destination == c {
            destination = self.safe_decrement(destination);
        }

        // The crab selects a new current cup: the cup which is immediately clockwise of the current
        // cup.
        self.selected = self.links[c];

        // The crab places the cups it just picked up so that they are immediately clockwise of the
        // destination cup. They keep the same order as when they were picked up.
        self.links[c] = self.links[destination];
        self.links[destination] = a;
    }

    fn short(string: &str) -> Cups {
        Cups::long(string, string.len())
    }

    fn long(string: &str, length: usize) -> Cups {
        assert!(length >= string.len());
        let mut links: Vec<usize> = Vec::with_capacity(length);

        for i in 0..length {
            links.push(i + 1)
        }

        let input: Vec<usize> = string
            .chars()
            .map(|ch| ch.to_string().parse::<usize>().unwrap() - 1)
            .collect();

        for i in 0..input.len() - 1 {
            let value = input[i];
            let points_to = input[i + 1];
            links[value] = points_to;
        }

        if input.len() == length {
            links[input[input.len() - 1]] = input[0];
        } else {
            links[input[input.len() - 1]] = input.len();
            let last_idx = links.len() - 1;
            links[last_idx] = input[0];
        }

        Cups {
            links,
            selected: input[0],
        }
    }
}

fn answer1(cups: &mut Cups) -> String {
    let mut result: Vec<usize> = vec![];
    let mut current = cups.links[0];
    while current != 0 {
        result.push(current);
        current = cups.links[current];
    }
    return result
        .iter()
        .map(|x| (x + 1).to_string())
        .collect::<String>();
}

fn answer2(cups: &mut Cups) -> usize {
    let a = cups.links[0];
    let b = cups.links[a];
    (a + 1) * (b + 1)
}

fn simulate(cups: &mut Cups, iterations: u32) {
    for _ in 0..iterations {
        cups.iteration();
    }
}

fn main() {
    let mut test_cups_short_1 = Cups::short("389125467");
    simulate(&mut test_cups_short_1, 10);
    assert_eq!(answer1(&mut test_cups_short_1), "92658374");

    let mut test_cups_short_2 = Cups::short("389125467");
    simulate(&mut test_cups_short_2, 100);
    assert_eq!(answer1(&mut test_cups_short_2), "67384529");

    let mut real_cups_short = Cups::short("942387615");
    simulate(&mut real_cups_short, 100);
    let real_answer_1 = answer1(&mut real_cups_short);
    println!("{real_answer_1}");
    assert_eq!(real_answer_1, "36542897");

    let mut test_cups_long = Cups::long("389125467", 1_000_000);
    simulate(&mut test_cups_long, 10_000_000);
    let answer_test_cups_long = answer2(&mut test_cups_long);
    assert_eq!(answer_test_cups_long, 149_245_887_792);

    let mut real_cups_long = Cups::long("942387615", 1_000_000);
    simulate(&mut real_cups_long, 10_000_000);
    let answer_real_cups_long = answer2(&mut real_cups_long);
    println!("{answer_real_cups_long}");
    assert_eq!(answer_real_cups_long, 562_136_730_660);
}
