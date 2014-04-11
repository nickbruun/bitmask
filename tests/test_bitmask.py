import six
from unittest import TestCase
from bitmask import BitMask


A_MASK = 1 << 0
B_MASK = 1 << 2
C_MASK = 1 << 1
D_MASK = 1 << 4
E_MASK = 1 << 3
ABCDE_MASK = 0b11111


integer_type = int
if six.PY3:
    long = None
if six.PY2:
    integer_type = long


class TestMask(BitMask):
    """Test mask.
    """

    a = A_MASK
    b = B_MASK
    c = C_MASK
    d = D_MASK
    e = E_MASK


class OtherTestMask(BitMask):
    """Other test mask.
    """

    a = A_MASK
    b = B_MASK
    c = C_MASK
    d = D_MASK
    e = E_MASK


class BitMaskTestCase(TestCase):
    """Test case for :class:`BitMask`.
    """

    def test_new(self):
        """class ..(BitMask):
        """

        # Constructing valid bit masks works as expected.
        class A(BitMask):
            pass

        class B(BitMask):
            a = 1 << 0
            b = 1 << 1
            c = 1 << 2
            d = 1 << 3

        # Constructing invalid bit masks throws expected error.
        with self.assertRaises(ValueError):
            class C(BitMask):
                a = 1 << 0
                b = 3

    def test_contains(self):
        """flag (not) in class ..(BitMask)
        """

        for flag_mask in [
                A_MASK,
                B_MASK,
                C_MASK,
                D_MASK,
                E_MASK,
        ]:
            self.assertIn(TestMask(flag_mask), TestMask)

        for flag_mask in [
                A_MASK,
                B_MASK,
                C_MASK,
                D_MASK,
                E_MASK,
        ]:
            self.assertNotIn(OtherTestMask(flag_mask), TestMask)

    def test_delattr(self):
        """del class ..(BitMask).flag
        """

        with self.assertRaises(AttributeError):
            del TestMask.a

    def test_dir(self):
        """dir(class ..(BitMask))
        """

        self.assertEqual(dir(TestMask), [
            '__class__',
            '__doc__',
            '__members__',
            '__module__',
            'a',
            'b',
            'c',
            'd',
            'e',
        ])

    def test_getattr(self):
        """class ..(BitMask).flag
        """

        self.assertEqual(TestMask.a, TestMask(A_MASK))
        self.assertEqual(TestMask.b, TestMask(B_MASK))
        self.assertEqual(TestMask.c, TestMask(C_MASK))
        self.assertEqual(TestMask.d, TestMask(D_MASK))
        self.assertEqual(TestMask.e, TestMask(E_MASK))

    def test_getitem(self):
        """class ..(BitMask)['name']
        """

        self.assertEqual(TestMask['a'], TestMask(A_MASK))
        self.assertEqual(TestMask['b'], TestMask(B_MASK))
        self.assertEqual(TestMask['c'], TestMask(C_MASK))
        self.assertEqual(TestMask['d'], TestMask(D_MASK))
        self.assertEqual(TestMask['e'], TestMask(E_MASK))

    def test_iter(self):
        """iter(class ..(BitMask))
        """

        self.assertEqual(list(iter(TestMask)), [
            TestMask.a,
            TestMask.c,
            TestMask.b,
            TestMask.e,
            TestMask.d,
        ])

    def test_reversed(self):
        """reversed(class ..(BitMask))
        """

        self.assertEqual(list(reversed(TestMask)), [
            TestMask.d,
            TestMask.e,
            TestMask.b,
            TestMask.c,
            TestMask.a,
        ])

    def test_len(self):
        """len(class ..(BitMask))
        """

        self.assertEqual(len(TestMask), 5)

    def test_repr(self):
        """repr(class ..(BitMask))
        """

        self.assertEqual(repr(TestMask), "<bit mask 'TestMask'>")

    def test_setattr(self):
        """class ..(BitMask).flag = ..
        """

        with self.assertRaises(AttributeError):
            TestMask.a = None

        with self.assertRaises(AttributeError):
            TestMask.a = TestMask(A_MASK)

    def test_init(self):
        """class ..(BitMask)(..)
        """

        # Valid value result in an instance with the expected value.
        for v in [
                0,
                A_MASK,
                B_MASK,
                C_MASK,
                D_MASK,
                E_MASK,
                ABCDE_MASK,
        ]:
            m = TestMask(v)
            self.assertEqual(m._value_, v)
            self.assertEqual(m.value, v)
            self.assertEqual(int(m), v)

        # Invalid value raises.
        for v in [
                -1,
                ABCDE_MASK + 1,
        ]:
            with self.assertRaises(ValueError):
                TestMask(v)

    def test_inst_repr(self):
        """repr(class ..(BitMask)(..))
        """

        for v, e in [
                (TestMask(0), '<TestMask: 0>'),
                (TestMask.a, '<TestMask.a: 1>'),
                (TestMask.a | TestMask.b | TestMask.e,
                 '<TestMask.a | TestMask.b | TestMask.e: 13>'),
        ]:
            self.assertEqual(repr(v), e)

    def test_inst_str(self):
        """str(class ..(BitMask)(..))
        """

        for v, e in [
                (TestMask(0), ''),
                (TestMask.a, 'TestMask.a'),
                (TestMask.a | TestMask.b | TestMask.e,
                 'TestMask.a | TestMask.b | TestMask.e'),
        ]:
            self.assertEqual(str(v), e)

    def test_inst_contains(self):
        """class ..(BitMask)(..) (not) in class ..(BitMask)(..)
        """

        for flag_mask in [
                A_MASK,
                B_MASK,
                C_MASK,
                D_MASK,
                E_MASK,
        ]:
            self.assertIn(TestMask(flag_mask), TestMask(ABCDE_MASK))
            self.assertNotIn(TestMask(flag_mask),
                             TestMask(ABCDE_MASK & ~(flag_mask)))

        for flag_mask in [
                A_MASK,
                B_MASK,
                C_MASK,
                D_MASK,
                E_MASK,
        ]:
            self.assertNotIn(OtherTestMask(flag_mask), TestMask(ABCDE_MASK))

    def test_int(self):
        """int(class ..(BitMask)(..))
        """

        for flag_mask in [
                A_MASK,
                B_MASK,
                C_MASK,
                D_MASK,
                E_MASK,
                ABCDE_MASK,
        ]:
            value = int(TestMask(flag_mask))
            self.assertIsInstance(value, int)
            self.assertEqual(value, flag_mask)

    if six.PY2:
        def test_long(self):
            """long(class ..(BitMask)(..))
            """

            for flag_mask in [
                    A_MASK,
                    B_MASK,
                    C_MASK,
                    D_MASK,
                    E_MASK,
                    ABCDE_MASK,
            ]:
                value = long(TestMask(flag_mask))
                self.assertIsInstance(value, long)
                self.assertEqual(value, flag_mask)

    def test_or(self):
        """class ..(BitMask)(..) | class ..(BitMask)(..)
        """

        # Valid values result in expected result.
        for a, b in [
                (TestMask(0), TestMask(0)),
                (TestMask(0), TestMask.a),
                (TestMask.a, TestMask.a),
                (TestMask.a, TestMask.b),
                (TestMask.a | TestMask.b, TestMask.c),
        ]:
            result = a | b
            self.assertEqual(result.value, a.value | b.value)

        # Invalid values result in exception.
        for a, b in [
                (TestMask.a, OtherTestMask.a),
        ]:
            with self.assertRaises(TypeError):
                a | b

    def test_and(self):
        """class ..(BitMask)(..) & class ..(BitMask)(..)
        """

        # Valid values result in expected result.
        for a, b in [
                (TestMask(0), TestMask(0)),
                (TestMask(0), TestMask.a),
                (TestMask.a, TestMask.a),
                (TestMask.a, TestMask.b),
                (TestMask.a | TestMask.b | TestMask.c, TestMask.b),
        ]:
            result = a & b
            self.assertEqual(result.value, a.value & b.value)

        # Invalid values result in exception.
        for a, b in [
                (TestMask.a, OtherTestMask.a),
        ]:
            with self.assertRaises(TypeError):
                a & b

    def test_xor(self):
        """class ..(BitMask)(..) ^ class ..(BitMask)(..)
        """

        # Valid values result in expected result.
        for a, b in [
                (TestMask(0), TestMask(0)),
                (TestMask(0), TestMask.a),
                (TestMask.a, TestMask.a),
                (TestMask.a, TestMask.b),
                (TestMask.a | TestMask.b | TestMask.c, TestMask.b),
        ]:
            result = a ^ b
            self.assertEqual(result.value, a.value ^ b.value)

        # Invalid values result in exception.
        for a, b in [
                (TestMask.a, OtherTestMask.a),
        ]:
            with self.assertRaises(TypeError):
                a ^ b

    def test_add(self):
        """class ..(BitMask)(..) + class ..(BitMask)(..)
        """

        # Valid values result in expected result.
        for a, b in [
                (TestMask(0), TestMask(0)),
                (TestMask(0), TestMask.a),
                (TestMask.a, TestMask.a),
                (TestMask.a, TestMask.b),
                (TestMask.a | TestMask.b, TestMask.c),
        ]:
            result = a + b
            self.assertEqual(result.value, a.value | b.value)

        # Invalid values result in exception.
        for a, b in [
                (TestMask.a, OtherTestMask.a),
        ]:
            with self.assertRaises(TypeError):
                a + b

    def test_sub(self):
        """class ..(BitMask)(..) - class ..(BitMask)(..)
        """

        # Valid values result in expected result.
        for a, b in [
                (TestMask(0), TestMask(0)),
                (TestMask(0), TestMask.a),
                (TestMask.a, TestMask(0)),
                (TestMask.a, TestMask.a),
                (TestMask.a, TestMask.b),
                (TestMask.a | TestMask.b, TestMask.c),
        ]:
            result = a - b
            self.assertEqual(result.value, a.value & ~b.value)

        # Invalid values result in exception.
        for a, b in [
                (TestMask.a, OtherTestMask.a),
        ]:
            with self.assertRaises(TypeError):
                a - b

    def test_ior(self):
        """class ..(BitMask)(..) |= class ..(BitMask)(..)
        """

        # Valid values result in expected result.
        for a, b in [
                (TestMask(0), TestMask(0)),
                (TestMask(0), TestMask.a),
                (TestMask.a, TestMask.a),
                (TestMask.a, TestMask.b),
                (TestMask.a | TestMask.b, TestMask.c),
        ]:
            result = a.__class__(a.value)
            result |= b
            self.assertEqual(result.value, a.value | b.value)

        # Invalid values result in exception.
        for a, b in [
                (TestMask.a, OtherTestMask.a),
        ]:
            with self.assertRaises(TypeError):
                result = a.__class__(a.value)
                result |= b

    def test_iand(self):
        """class ..(BitMask)(..) &= class ..(BitMask)(..)
        """

        # Valid values result in expected result.
        for a, b in [
                (TestMask(0), TestMask(0)),
                (TestMask(0), TestMask.a),
                (TestMask.a, TestMask.a),
                (TestMask.a, TestMask.b),
                (TestMask.a | TestMask.b | TestMask.c, TestMask.b),
        ]:
            result = a.__class__(a.value)
            result &= b
            self.assertEqual(result.value, a.value & b.value)

        # Invalid values result in exception.
        for a, b in [
                (TestMask.a, OtherTestMask.a),
        ]:
            with self.assertRaises(TypeError):
                result = a.__class__(a.value)
                result &= b

    def test_ixor(self):
        """class ..(BitMask)(..) ^= class ..(BitMask)(..)
        """

        # Valid values result in expected result.
        for a, b in [
                (TestMask(0), TestMask(0)),
                (TestMask(0), TestMask.a),
                (TestMask.a, TestMask.a),
                (TestMask.a, TestMask.b),
                (TestMask.a | TestMask.b | TestMask.c, TestMask.b),
        ]:
            result = a.__class__(a.value)
            result ^= b
            self.assertEqual(result.value, a.value ^ b.value)

        # Invalid values result in exception.
        for a, b in [
                (TestMask.a, OtherTestMask.a),
        ]:
            with self.assertRaises(TypeError):
                result = a.__class__(a.value)
                result ^= b

    def test_iadd(self):
        """class ..(BitMask)(..) += class ..(BitMask)(..)
        """

        # Valid values result in expected result.
        for a, b in [
                (TestMask(0), TestMask(0)),
                (TestMask(0), TestMask.a),
                (TestMask.a, TestMask.a),
                (TestMask.a, TestMask.b),
                (TestMask.a | TestMask.b, TestMask.c),
        ]:
            result = a.__class__(a.value)
            result += b
            self.assertEqual(result.value, a.value | b.value)

        # Invalid values result in exception.
        for a, b in [
                (TestMask.a, OtherTestMask.a),
        ]:
            with self.assertRaises(TypeError):
                result = a.__class__(a.value)
                result += b

    def test_isub(self):
        """class ..(BitMask)(..) -= class ..(BitMask)(..)
        """

        # Valid values result in expected result.
        for a, b in [
                (TestMask(0), TestMask(0)),
                (TestMask(0), TestMask.a),
                (TestMask.a, TestMask(0)),
                (TestMask.a, TestMask.a),
                (TestMask.a, TestMask.b),
                (TestMask.a | TestMask.b, TestMask.c),
        ]:
            result = a.__class__(a.value)
            result -= b
            self.assertEqual(result.value, a.value & ~b.value)

        # Invalid values result in exception.
        for a, b in [
                (TestMask.a, OtherTestMask.a),
        ]:
            with self.assertRaises(TypeError):
                result = a.__class__(a.value)
                result -= b

    def test_eq(self):
        """class ..(BitMask)(..) == class ..(BitMask)(..)
        """

        for a, b in [
                (TestMask(0), TestMask(0)),
                (TestMask.a, TestMask.a),
                (TestMask.a | TestMask.b, TestMask.b | TestMask.a),
        ]:
            self.assertTrue(a == b,
                            'expected %r == %r' % (a, b))

        for a, b in [
                (TestMask(0), TestMask.a),
                (TestMask.a, TestMask.b),
                (TestMask.a | TestMask.b, TestMask.b | TestMask.c),
                (OtherTestMask.a, TestMask.a),
        ]:
            self.assertFalse(a == b,
                             'expected not %r == %r' % (a, b))

    def test_ne(self):
        """class ..(BitMask)(..) == class ..(BitMask)(..)
        """

        for a, b in [
                (TestMask(0), TestMask(0)),
                (TestMask.a, TestMask.a),
                (TestMask.a | TestMask.b, TestMask.b | TestMask.a),
        ]:
            self.assertFalse(a != b,
                             'expected not %r != %r' % (a, b))

        for a, b in [
                (TestMask(0), TestMask.a),
                (TestMask.a, TestMask.b),
                (TestMask.a | TestMask.b, TestMask.b | TestMask.c),
                (OtherTestMask.a, TestMask.a),
        ]:
            self.assertTrue(a != b,
                            'expected %r != %r' % (a, b))

    def test_nonzero(self):
        """bool(class ..(BitMask)(..))
        """

        for v, e in [
                (TestMask(0), False),
                (TestMask.a, True),
                (TestMask.a | TestMask.b | TestMask.c, True),
        ]:
            self.assertEqual(bool(v),
                             e,
                             'expected bool(%r) to be %r' % (v, e))

    def test_add_method(self):
        """class ..(BitMask)(..).add(class ..(BitMask)(..))
        """

        # Valid values result in expected result.
        for a, b in [
                (TestMask(0), TestMask(0)),
                (TestMask(0), TestMask.a),
                (TestMask.a, TestMask.a),
                (TestMask.a, TestMask.b),
                (TestMask.a | TestMask.b, TestMask.c),
        ]:
            result = a.__class__(a.value)
            result.add(b)
            self.assertEqual(result.value, a.value | b.value)

        # Invalid values result in exception.
        for a, b in [
                (TestMask.a, OtherTestMask.a),
        ]:
            with self.assertRaises(TypeError):
                result = a.__class__(a.value)
                result.add(b)

    def test_remove(self):
        """class ..(BitMask)(..).remove(class ..(BitMask)(..))
        """

        # Valid values result in expected result.
        for a, b in [
                (TestMask(0), TestMask(0)),
                (TestMask(0), TestMask.a),
                (TestMask.a, TestMask(0)),
                (TestMask.a, TestMask.a),
                (TestMask.a, TestMask.b),
                (TestMask.a | TestMask.b, TestMask.c),
        ]:
            result = a.__class__(a.value)
            result.remove(b)
            self.assertEqual(result.value, a.value & ~b.value)

        # Invalid values result in exception.
        for a, b in [
                (TestMask.a, OtherTestMask.a),
        ]:
            with self.assertRaises(TypeError):
                result = a.__class__(a.value)
                result.remove(b)

    def test_value(self):
        """class ..(BitMask)(..).value
        """

        for v, e in [
                (TestMask(0), 0),
                (TestMask.a, A_MASK),
                (TestMask.a | TestMask.b | TestMask.c,
                 A_MASK | B_MASK | C_MASK),
        ]:
            a = v.value
            self.assertIsInstance(a, integer_type)
            self.assertEqual(a, e)

    def test_flags(self):
        """class ..(BitMask)(..).flags
        """

        for v, e in [
                (TestMask(0), []),
                (TestMask.a, [TestMask.a]),
                (TestMask.a | TestMask.b | TestMask.c,
                 [TestMask.a, TestMask.c, TestMask.b]),
        ]:
            self.assertEqual(v.flags, e)

    def test_name(self):
        """class ..(BitMask)(..).name
        """

        for v, e in [
                (TestMask.a, 'a'),
                (TestMask.b, 'b'),
                (TestMask.c, 'c'),
                (TestMask.d, 'd'),
                (TestMask.e, 'e'),
        ]:
            self.assertEqual(v.name, e)

        for v in [
                TestMask(0),
                TestMask.a | TestMask.b,
        ]:
            with self.assertRaises(AttributeError):
                v.name

    def test_hash(self):
        """hash(class ..(BitMask)(..))
        """

        hashes = [hash(m) for m in [
            TestMask.a,
            TestMask.b,
            TestMask.c,
            TestMask.d,
            TestMask.e,
            OtherTestMask.a,
            OtherTestMask.b,
            OtherTestMask.c,
            OtherTestMask.d,
            OtherTestMask.e,
        ]]

        self.assertEqual(len(hashes), len(set(hashes)))

    def test_lt(self):
        """class ..(BitMask)(..) < class ..(BitMask)(..)
        """

        cls_cmp = TestMask < OtherTestMask

        for a, b, e in [
                (TestMask.a, TestMask.a, False),
                (TestMask.a, TestMask.b, True),
                (TestMask.a, TestMask.c, True),
                (TestMask.a, TestMask.d, True),
                (TestMask.a, TestMask.e, True),
                (TestMask.b, TestMask.a, False),
                (TestMask.c, TestMask.a, False),
                (TestMask.d, TestMask.a, False),
                (TestMask.e, TestMask.a, False),
                (TestMask.a, OtherTestMask.a, cls_cmp),
                (TestMask.b, OtherTestMask.b, cls_cmp),
                (TestMask.c, OtherTestMask.c, cls_cmp),
                (TestMask.d, OtherTestMask.d, cls_cmp),
                (TestMask.e, OtherTestMask.e, cls_cmp),
                (OtherTestMask.a, TestMask.a, not cls_cmp),
                (OtherTestMask.b, TestMask.b, not cls_cmp),
                (OtherTestMask.c, TestMask.c, not cls_cmp),
                (OtherTestMask.d, TestMask.d, not cls_cmp),
                (OtherTestMask.e, TestMask.e, not cls_cmp),
        ]:
            self.assertEqual(a < b,
                             e,
                             'expected %r < %r to be %r' % (a, b, e))

    def test_le(self):
        """class ..(BitMask)(..) <= class ..(BitMask)(..)
        """

        cls_cmp = TestMask <= OtherTestMask

        for a, b, e in [
                (TestMask.a, TestMask.a, True),
                (TestMask.a, TestMask.b, True),
                (TestMask.a, TestMask.c, True),
                (TestMask.a, TestMask.d, True),
                (TestMask.a, TestMask.e, True),
                (TestMask.b, TestMask.a, False),
                (TestMask.c, TestMask.a, False),
                (TestMask.d, TestMask.a, False),
                (TestMask.e, TestMask.a, False),
                (TestMask.a, OtherTestMask.a, cls_cmp),
                (TestMask.b, OtherTestMask.b, cls_cmp),
                (TestMask.c, OtherTestMask.c, cls_cmp),
                (TestMask.d, OtherTestMask.d, cls_cmp),
                (TestMask.e, OtherTestMask.e, cls_cmp),
                (OtherTestMask.a, TestMask.a, not cls_cmp),
                (OtherTestMask.b, TestMask.b, not cls_cmp),
                (OtherTestMask.c, TestMask.c, not cls_cmp),
                (OtherTestMask.d, TestMask.d, not cls_cmp),
                (OtherTestMask.e, TestMask.e, not cls_cmp),
        ]:
            self.assertEqual(a <= b,
                             e,
                             'expected %r <= %r to be %r' % (a, b, e))

    def test_gt(self):
        """class ..(BitMask)(..) > class ..(BitMask)(..)
        """

        cls_cmp = TestMask > OtherTestMask

        for a, b, e in [
                (TestMask.a, TestMask.a, False),
                (TestMask.a, TestMask.b, False),
                (TestMask.a, TestMask.c, False),
                (TestMask.a, TestMask.d, False),
                (TestMask.a, TestMask.e, False),
                (TestMask.b, TestMask.a, True),
                (TestMask.c, TestMask.a, True),
                (TestMask.d, TestMask.a, True),
                (TestMask.e, TestMask.a, True),
                (TestMask.a, OtherTestMask.a, cls_cmp),
                (TestMask.b, OtherTestMask.b, cls_cmp),
                (TestMask.c, OtherTestMask.c, cls_cmp),
                (TestMask.d, OtherTestMask.d, cls_cmp),
                (TestMask.e, OtherTestMask.e, cls_cmp),
                (OtherTestMask.a, TestMask.a, not cls_cmp),
                (OtherTestMask.b, TestMask.b, not cls_cmp),
                (OtherTestMask.c, TestMask.c, not cls_cmp),
                (OtherTestMask.d, TestMask.d, not cls_cmp),
                (OtherTestMask.e, TestMask.e, not cls_cmp),
        ]:
            self.assertEqual(a > b,
                             e,
                             'expected %r > %r to be %r' % (a, b, e))

    def test_ge(self):
        """class ..(BitMask)(..) >= class ..(BitMask)(..)
        """

        cls_cmp = TestMask >= OtherTestMask

        for a, b, e in [
                (TestMask.a, TestMask.a, True),
                (TestMask.a, TestMask.b, False),
                (TestMask.a, TestMask.c, False),
                (TestMask.a, TestMask.d, False),
                (TestMask.a, TestMask.e, False),
                (TestMask.b, TestMask.a, True),
                (TestMask.c, TestMask.a, True),
                (TestMask.d, TestMask.a, True),
                (TestMask.e, TestMask.a, True),
                (TestMask.a, OtherTestMask.a, cls_cmp),
                (TestMask.b, OtherTestMask.b, cls_cmp),
                (TestMask.c, OtherTestMask.c, cls_cmp),
                (TestMask.d, OtherTestMask.d, cls_cmp),
                (TestMask.e, OtherTestMask.e, cls_cmp),
                (OtherTestMask.a, TestMask.a, not cls_cmp),
                (OtherTestMask.b, TestMask.b, not cls_cmp),
                (OtherTestMask.c, TestMask.c, not cls_cmp),
                (OtherTestMask.d, TestMask.d, not cls_cmp),
                (OtherTestMask.e, TestMask.e, not cls_cmp),
        ]:
            self.assertEqual(a >= b,
                             e,
                             'expected %r >= %r to be %r' % (a, b, e))
