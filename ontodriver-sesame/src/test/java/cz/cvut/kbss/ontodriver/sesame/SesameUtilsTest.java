package cz.cvut.kbss.ontodriver.sesame;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.openrdf.model.Literal;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.vocabulary.XMLSchema;
import org.openrdf.sail.memory.MemoryStore;

import static org.junit.Assert.*;

/**
 * @author ledvima1
 */
public class SesameUtilsTest {

    private static final String LANG = "en";

    private static ValueFactory vf;

    private static MemoryStore memoryStore;

    private enum Severity {
        LOW, MEDIUM
    }

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        memoryStore = new MemoryStore();
        memoryStore.initialize();
        vf = memoryStore.getValueFactory();
    }

    @AfterClass
    public static void tearDownAfterClass() throws Exception {
        memoryStore.shutDown();
    }

    @Test
    public void enumLiteralIsReturnedAsStringValue() throws Exception {
        final Literal literal = vf.createLiteral(Severity.LOW.toString(), LANG);
        final Object result = SesameUtils.getDataPropertyValue(literal);
        assertEquals(Severity.LOW.toString(), result);
    }

    @Test
    public void enumValueIsReturnedAsStringLiteral() throws Exception {
        final Literal literal = SesameUtils.createDataPropertyLiteral(Severity.MEDIUM, LANG, vf);
        assertNotNull(literal);
        assertEquals(Severity.MEDIUM.toString(), literal.stringValue());
        assertTrue(literal.getDatatype() == null || literal.getDatatype().equals(XMLSchema.STRING));
    }
}