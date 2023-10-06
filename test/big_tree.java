class GiantTree {
    public static void main(String[] a) {   
        System.out.println(new Tree().CreateAndSum(20));
    }
}

class Tree {
    Tree left;
    Tree right;
    int key;
    boolean has_left;
    boolean has_right;

    public int CreateAndSum(int depth) {
        int t;
        int s;
        int n;
        Tree tree;
        s = 0;
        n = 0;
        while (n < 20) {
            tree = new Tree();
            t = tree.CreateTreeOfDepth(depth, 1);
            s = s + tree.Sum();
            n = n + 1;
        }
        return s;
    }

    public int Sum() {
        int ret;
        int l;
        int r;
        ret = key;
        if (has_left) {
            l = left.Sum();
            ret = ret + l;
        } else {
            // do nothing
        }
        if (has_right) {
            r = right.Sum();
            ret = ret + r;
        } else {
            // do nothing
        }
        return ret;
    }

    public int CreateTreeOfDepth(int depth, int v) {
        int dummy;
        int d;
        Tree t;
        dummy = 0;
        key = v;
        if (depth < 1) {
            has_left = false;
            has_right = false;
        } else {
            d = depth - 1;
            t = new Tree();
            dummy = t.CreateTreeOfDepth(d, v);
            left = t;
            t = new Tree();
            dummy = t.CreateTreeOfDepth(d, v);
            right = t;
            has_left = true;
            has_right = true;
        }
        return dummy;
    }
}
