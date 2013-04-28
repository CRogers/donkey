import Prop (Ref, Prop(..))

-- Syntax

data Visser =
    VT | VF
  | VC Visser Visser
  | VD Visser Visser
  | VE Ref
  | VP Ref [Ref]
  | VSw
  | VSc
  | VQ0 Visser
  | VQ1 Visser
  deriving (Eq, Show)

-- Interpretation
{-
type IVisser = (DPL, DPL, DPL, DPL, Integer, Integer)

intVisser :: Visser -> IVisser
intVisser VT = (DT, DT, DT, DT, 1, 0)
intVisser VF = (DT, DT, DT, DF, 1, 0)
intVisser VSw = (DT, DT, DT, DT, -1, 0)
intVisser VSc = (DT, DT, DT, DT, 1, 1)
intVisser (VP p rs) = (DT, DT, DT, DPredi p rs, 1, 0)
intVisser (VE r) = (DT, DT, DT, DExists r, 1, 0)
intVisser (VC vis1 vis2) = compVisser (intVisser vis1) (intVisser vis2)
intVisser (VQ0 vis) = (qm1, DT, qp1, DImpl qm0 qp0, 1, 0)
    where (qm1, qm0, qp1, qp0, a, i) = intVisser vis
intVisser (VQ1 vis) = (DT, DT, DT, DImpl (DComp qm1 qm0) (DComp qp1 qp0), 1, 0)
    where (qm1, qm0, qp1, qp0, a, i) = intVisser vis

compVisser :: IVisser -> IVisser -> IVisser
compVisser (qm1, qm0, qp1, qp0, 1, 0) (rm1, rm0, rp1, rp0, b, j)  = (DComp qm1 rm1, DComp qm0 rm0, DComp qp1 rp1, DComp qp0 rp0, b, j)
compVisser (qm1, qm0, qp1, qp0, 1, 1) (rm1, rm0, rp1, rp0, b, j)  = (DComp qm1 rm0, DComp qm0 rm1, DComp qp1 rp0, DComp qp0 rp1, b, 1-j)
compVisser (qm1, qm0, qp1, qp0, -1, 0) (rm1, rm0, rp1, rp0, b, j) = (DComp qm1 rp1, DComp qm0 rp0, DComp qp1 rm1, DComp qp0 rm0, -b, j)
compVisser (qm1, qm0, qp1, qp0, -1, 1) (rm1, rm0, rp1, rp0, b, j) = (DComp qm1 rp0, DComp qm0 rp1, DComp qp1 rm0, DComp qp0 rm1, -b, 1-j)

-- Tools

visser2Dpl :: IVisser -> DPL
visser2Dpl (qm1, qm0, qp1, qp0, a, i) = DImpl (DComp qm1 qm0) (DComp qp1 qp0)

tex :: Prop -> (String, Int)
tex VT = "\\top"
tex VF = "\\bot"
tex VSw = "\\bowtie"
tex VSc = "\\triangle"
tex (VC p q) = tex p ++ " \\cdot " ++ tex q
tex (VD p q) = "(" ++ tex p ++ ") \\vee (" ++ tex q ++ ")"
tex (VE k) = "E" ++ k
tex (VP f ks) = f ++ "(" ++ concat (intersperse "," ks) ++ ")"
tex (VQ0 p) = "?_0(" ++ tex p ++ ")"
tex (VQ1 p) = "?_1(" ++ tex p ++ ")"

tostring :: Prop -> String
tostring = fst . tex
-}