use std::collections::{HashMap, HashSet};
use std::hash::{BuildHasher, Hash};

pub fn create_adjacency_hashmap<T: Hash + Eq + Clone>(
    undirected_edges: &[(T, T)],
) -> HashMap<T, HashSet<T>> {
    let mut connections: HashMap<T, HashSet<T>> = HashMap::new();

    for (a, b) in undirected_edges {
        connections.entry(a.clone()).or_default().insert(b.clone());
        connections.entry(b.clone()).or_default().insert(a.clone());
    }

    connections
}

fn bron_kerbosch<F, T: Eq + Hash + Copy, S: BuildHasher + Default + Clone>(
    neighbours: &HashMap<T, HashSet<T, S>, S>,
    r: &mut HashSet<T, S>,
    mut p: HashSet<T, S>,
    mut x: HashSet<T, S>,
    found: &mut F,
) where
    F: FnMut(&HashSet<T, S>),
{
    // if P and X are both empty then
    if p.is_empty() && x.is_empty() {
        // report R as a maximal clique
        found(r);
    } else {
        // choose a pivot vertex u in P ⋃ X
        let p_union_x = p.union(&x);
        let u = *p_union_x
            .into_iter()
            .max_by_key(|v| {
                // Not sure if this is always the best, perhaps a random element is sometimes better?
                neighbours.get(v).map_or(0, |n_v| n_v.iter().count())
            })
            .expect("p_union_x is not empty");

        let p_minus_n_u: HashSet<T> = p
            .iter()
            .filter(|x| !neighbours.get(&u).is_some_and(|set| set.contains(x)))
            .copied()
            .collect();

        // for each vertex v in P \ N(u) do
        for v in p_minus_n_u {
            let n_v = neighbours.get(&v).cloned().unwrap_or_else(HashSet::default);
            r.insert(v);
            // BronKerbosch2(R ⋃ {v}, P ⋂ N(v), X ⋂ N(v))
            bron_kerbosch(
                neighbours,
                r,
                p.intersection(&n_v).copied().collect(),
                x.intersection(&n_v).copied().collect(),
                found,
            );
            r.remove(&v);
            p.remove(&v);
            x.insert(v);
        }
    }
}

// https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm
pub fn enumerate_maximum_cliques<F, T: Eq + Hash + Copy, S: BuildHasher + Default + Clone>(
    neighbours: &HashMap<T, HashSet<T, S>, S>,
    found: &mut F,
) where
    F: FnMut(&HashSet<T, S>),
{
    bron_kerbosch(
        neighbours,
        &mut HashSet::default(),
        neighbours.keys().copied().collect::<HashSet<T, S>>(),
        HashSet::default(),
        found,
    );
}
