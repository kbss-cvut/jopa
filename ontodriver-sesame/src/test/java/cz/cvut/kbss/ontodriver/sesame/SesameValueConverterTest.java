package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import org.junit.Before;
import org.junit.Test;
import org.openrdf.model.Literal;
import org.openrdf.model.Value;
import org.openrdf.model.impl.ValueFactoryImpl;

import java.net.URI;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class SesameValueConverterTest {

    private static final String LANG = "en";
    private static final URI PROPERTY = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#property");

    private SesameValueConverter converter;

    @Before
    public void setUp() {
        this.converter = new SesameValueConverter(new ValueFactoryImpl(), LANG);
    }

    @Test
    public void convertsObjectPropertyNamedResourceToSesameUri() throws Exception {
        final Value res = converter.toSesameValue(assertion(Assertion.AssertionType.OBJECT_PROPERTY),
                value(NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa#individual")));
        assertTrue(res instanceof org.openrdf.model.URI);
    }

    private Assertion assertion(Assertion.AssertionType type) {
        switch (type) {
            case CLASS:
                return Assertion.createClassAssertion(false);
            case PROPERTY:
                return Assertion.createPropertyAssertion(PROPERTY, false);
            case OBJECT_PROPERTY:
                return Assertion.createObjectPropertyAssertion(PROPERTY, false);
            case DATA_PROPERTY:
                return Assertion.createDataPropertyAssertion(PROPERTY, false);
            case ANNOTATION_PROPERTY:
                return Assertion.createAnnotationPropertyAssertion(PROPERTY, false);
            default:
                throw new IllegalArgumentException("Unknown assertion type: " + type);
        }
    }

    private <T> cz.cvut.kbss.ontodriver.model.Value<T> value(T val) {
        return new cz.cvut.kbss.ontodriver.model.Value<>(val);
    }

    @Test
    public void convertsDataPropertyValueToSesameLiteral() throws Exception {
        final int val = 117;
        final Value res = converter.toSesameValue(assertion(Assertion.AssertionType.DATA_PROPERTY), value(val));
        assertTrue(res instanceof Literal);
        assertEquals(val, ((Literal) res).intValue());
    }

    @Test
    public void convertsAnnotationPropertyLiteralValueToSesameLiteral() throws Exception {
        final String val = "AnnotationValue";
        final Value res = converter.toSesameValue(assertion(Assertion.AssertionType.ANNOTATION_PROPERTY), value(val));
        assertTrue(res instanceof Literal);
        assertEquals(val, ((Literal) res).getLabel());
        assertEquals(LANG, ((Literal) res).getLanguage());
    }

    @Test
    public void convertsAnnotationPropertyNamedResourceToSesameUri() throws Exception {
        final NamedResource val = NamedResource
                .create("http://krizik.felk.cvut.cz/ontologies/jopa#individualAnnotation");
        final Value res = converter.toSesameValue(assertion(Assertion.AssertionType.ANNOTATION_PROPERTY), value(val));
        assertTrue(res instanceof org.openrdf.model.URI);
        assertEquals(val.toString(), res.toString());
    }

    @Test(expected = SesameDriverException.class)
    public void conversionThrowsExceptionWhenObjectPropertyValueIsNotUri() throws Exception {
        converter.toSesameValue(assertion(Assertion.AssertionType.OBJECT_PROPERTY), value(117));
    }
}