// Printing the graph into a dot repersentation. Notes the graph should be
// linked properly for printing purpose.
use crate::jit::node::*;

// Helper to print each component of the node, notes the output of these are
// been serialized into a list of dot's graph representation's attributes.
//
// the format rules are as following :
//
// 1) the op's op_tier will be reflected as different types/shape of the nodes
//
// 2) the op's opcode will be reflected as label, ie node name

fn p_op(x: &Oref, nid: Nid) -> String {
    let shape_string = match x.tier {
        OpTier::Imm => "hexagon",
        OpTier::Bval => "octagon",
        OpTier::Cfg => "box",
        OpTier::Rval => "circle",
        _ => "diamon",
    };

    let shape_color = match x.tier {
        OpTier::Imm => "aliceblue",
        OpTier::Cfg => "cyan",
        OpTier::Snapshot => "cornsilk",
        OpTier::Pseudo => "darkgrey",
        OpTier::Phi => "cornsilk4",
        OpTier::Bval => "gold",
        OpTier::Rval => "deepskyblue3",
        OpTier::Mid => "deepskyblue2",
        OpTier::Arch => "deepskyblue1",
        _ => "gray2",
    };

    return format!(
        "shape=\"{}\" color=\"{}\" label=\"{:?}[{}]\"",
        shape_string, shape_color, x.op, nid
    );
}

fn p_imm(x: &Imm) -> String {
    return format!("imm=\"{:?}\"", x);
}

fn nm(nid: Nid) -> String {
    return format!("node_{}", nid);
}

struct Nprinter {
    start: Nref,

    // temporary result
    node_define_part: String,
    node_link_part: String,
}

impl Nprinter {
    fn p_node(x: &Nref) -> String {
        let mut tmp = Vec::<String>::new();

        // name of the node
        tmp.push(format!("{} ", nm(x.borrow().id)));

        // attributes of the node
        tmp.push("[".to_string());
        tmp.push(p_op(&x.borrow().op, x.borrow().id));

        // rest attributes
        // (0) imm
        tmp.push(" ".to_string());
        tmp.push(p_imm(&x.borrow().imm));

        // (1) id
        tmp.push(format!(" id={}", x.borrow().id));

        // (2) bcid
        tmp.push(format!(" bcid={}", x.borrow().bcid));

        // (3) dead
        tmp.push(format!(" dead={}", x.borrow().dead));

        tmp.push("]".to_string());

        return tmp.join("");
    }

    // First pass, define the nodes in the graph's
    fn pass_define_node(&mut self) {
        let mut buf = Vec::<String>::new();
        let mut wq = Nqueue::new();
        wq.push_front(Nref::clone(&self.start));

        while wq.len() != 0 {
            let front = wq.pop_front().unwrap();
            buf.push(Nprinter::p_node(&front));

            for xx in front.borrow().value.iter() {
                wq.push_back(Nref::clone(&xx));
            }

            for xx in front.borrow().cfg.iter() {
                wq.push_back(Nref::clone(&xx));
            }
            for xx in front.borrow().effect.iter() {
                wq.push_back(Nref::clone(&xx));
            }
        }

        self.node_define_part = buf.join("\n");
    }

    // Second pass, define nodes's link
    fn pass_define_link(&mut self) {
        let mut buf = Vec::<String>::new();
        let mut wq = Nqueue::new();
        wq.push_front(Nref::clone(&self.start));

        while wq.len() != 0 {
            let front = wq.pop_front().unwrap();
            let name = nm(front.borrow().id);
            for xx in front.borrow().value.iter() {
                let tname = nm(xx.borrow().id);
                buf.push(format!("{} -> {} [color=\"black\"]", &name, &tname));
                wq.push_back(Nref::clone(&xx));
            }
            for xx in front.borrow().cfg.iter() {
                let tname = nm(xx.borrow().id);
                buf.push(format!("{} -> {} [color=\"red\"]", &name, &tname));
                wq.push_back(Nref::clone(&xx));
            }
            for xx in front.borrow().effect.iter() {
                let tname = nm(xx.borrow().id);
                buf.push(format!("{} -> {} [color=\"green\"]", &name, &tname));
                wq.push_back(Nref::clone(&xx));
            }
        }

        self.node_link_part = buf.join("\n");
    }

    fn print(&mut self) -> String {
        self.pass_define_node();
        self.pass_define_link();
        return format!(
            "digraph xxx {{\n{}\n{}\n}}",
            self.node_define_part, self.node_link_part
        );
    }

    fn new(s: Nref) -> Nprinter {
        Nprinter {
            start: s,
            node_define_part: "".to_string(),
            node_link_part: "".to_string(),
        }
    }
}

pub fn print_graph(x: &Nref) -> String {
    assert!(x.borrow().is_cfg_start());
    let mut printer = Nprinter::new(Nref::clone(x));
    return printer.print();
}
