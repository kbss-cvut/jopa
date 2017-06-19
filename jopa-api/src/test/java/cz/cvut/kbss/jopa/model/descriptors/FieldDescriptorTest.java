package cz.cvut.kbss.jopa.model.descriptors;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import java.net.URI;
import java.util.Arrays;
import java.util.Collection;

import static org.junit.Assert.*;

@RunWith(Parameterized.class)
public class FieldDescriptorTest {

    private static final URI CONTEXT_ONE = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/contextOne");
    private static final URI CONTEXT_TWO = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/contextTwo");

    @Parameterized.Parameters
    public static Collection<Object[]> data() {
        return Arrays.asList(new Object[][]{
                {null, null, "en", "cs", false}, {CONTEXT_ONE, CONTEXT_TWO, "en", "en", false},
                {null, null, "en", "en", true}, {CONTEXT_ONE, CONTEXT_ONE, "en", "en", true}
        });
    }

    @Parameterized.Parameter
    public URI contextOne;
    @Parameterized.Parameter(1)
    public URI contextTwo;
    @Parameterized.Parameter(2)
    public String langOne;
    @Parameterized.Parameter(3)
    public String langTwo;
    @Parameterized.Parameter(4)
    public boolean shouldBeEqual;

    @Test
    public void testEquality() throws Exception {
        final Descriptor dOne = new FieldDescriptor(contextOne, TestClass.stringAttField());
        final Descriptor dTwo = new FieldDescriptor(contextTwo, TestClass.stringAttField());
        dOne.setLanguage(langOne);
        dTwo.setLanguage(langTwo);
        if (shouldBeEqual) {
            assertEquals(dOne, dTwo);
        } else {
            assertNotEquals(dOne, dTwo);
        }
    }
}