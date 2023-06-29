/**
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.rdf4j;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.rdf4j.environment.Generator;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import org.eclipse.rdf4j.model.Literal;
import org.eclipse.rdf4j.model.Value;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.impl.SimpleValueFactory;
import org.junit.jupiter.api.Test;

import java.net.URI;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;


class ValueConverterTest {

    private static final String LANG = "en";
    private static final URI PROPERTY = Generator.generateUri();

    private final ValueFactory vf = SimpleValueFactory.getInstance();

    private final ValueConverter sut = new ValueConverter(SimpleValueFactory.getInstance());

    @Test
    void convertsObjectPropertyNamedResourceToRdf4jIri() throws Exception {
        final Value res = sut.toRdf4jValue(assertion(Assertion.AssertionType.OBJECT_PROPERTY),
                                           value(NamedResource.create(Generator.generateUri())));
        assertTrue(res.isIRI());
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
    void convertsDataPropertyValueToRdf4jLiteral() throws Exception {
        final int val = 117;
        final Value res = sut.toRdf4jValue(assertion(Assertion.AssertionType.DATA_PROPERTY), value(val));
        assertTrue(res.isLiteral());
        assertEquals(vf.createLiteral(val), res);
    }

    @Test
    void convertsAnnotationPropertyLiteralValueToRdf4jLiteral() throws Exception {
        final String val = "AnnotationValue";
        final Value res = sut.toRdf4jValue(assertion(Assertion.AssertionType.ANNOTATION_PROPERTY), value(val));
        assertTrue(res.isLiteral());
        assertEquals(vf.createLiteral(val), res);
    }

    @Test
    void convertsAnnotationPropertyNamedResourceToRdf4jIri() throws Exception {
        final NamedResource val = NamedResource.create(Generator.generateUri());
        final Value res = sut.toRdf4jValue(assertion(Assertion.AssertionType.ANNOTATION_PROPERTY), value(val));
        assertTrue(res.isIRI());
        assertEquals(vf.createIRI(val.toString()), res);
    }

    @Test
    void conversionThrowsExceptionWhenObjectPropertyValueIsNotUri() {
        assertThrows(Rdf4jDriverException.class,
                     () -> sut.toRdf4jValue(assertion(Assertion.AssertionType.OBJECT_PROPERTY), value(117)));
    }

    @Test
    void convertsStringLiteralIntoValueWithLanguageTagSpecifiedInAssertion() throws Exception {
        final String value = "hodnota v cestine";
        final Assertion dpAssertion = Assertion.createDataPropertyAssertion(PROPERTY, LANG, false);
        final Value res = sut.toRdf4jValue(dpAssertion, value(value));
        assertTrue(res.isLiteral());
        assertEquals(vf.createLiteral(value, LANG), res);
    }

    @Test
    void convertsStringLiteralIntoValueWithoutLanguageWhenAssertionHasNoLanguage() throws Exception {
        final String value = "hodnota v cestine";
        final Assertion dpAssertion = Assertion.createDataPropertyAssertion(PROPERTY, false);
        final Value res = sut.toRdf4jValue(dpAssertion, value(value));
        assertTrue(res.isLiteral());
        assertFalse(((Literal) res).getLanguage().isPresent());
        assertEquals(vf.createLiteral(value), res);
    }

    @Test
    void convertsAnnotationLiteralIntoValueWithLanguageTagSpecifiedInAssertion() throws Exception {
        final String value = "hodnota v cestine";
        final Assertion apAssertion = Assertion.createAnnotationPropertyAssertion(PROPERTY, LANG, false);
        final Value res = sut.toRdf4jValue(apAssertion, value(value));
        assertTrue(res.isLiteral());
        assertEquals(vf.createLiteral(value, LANG), res);
    }

    @Test
    void convertsAnnotationLiteralIntoValueWithoutLanguageWhenAssertionHasNoLanguage() throws Exception {
        final String value = "hodnota v cestine";
        final Assertion apAssertion = Assertion.createAnnotationPropertyAssertion(PROPERTY, false);
        final Value res = sut.toRdf4jValue(apAssertion, value(value));
        assertTrue(res.isLiteral());
        assertFalse(((Literal) res).getLanguage().isPresent());
        assertEquals(vf.createLiteral(value), res);
    }

    @Test
    void convertsAnnotationLiteralRepresentingUriToStringLiteralWithLanguage() throws Exception {
        final String value = Generator.generateUri().toString();
        final Assertion apAssertion = Assertion.createAnnotationPropertyAssertion(PROPERTY, LANG,false);
        final Value res = sut.toRdf4jValue(apAssertion, value(value));
        assertTrue(res.isLiteral());
        assertEquals(vf.createLiteral(value, LANG), res);
    }

    @Test
    void convertsAnnotationStringLiteralContainingColonIntoValueWithoutLanguageWhenAssertionHasNoLanguage() throws Exception {
        final String value = "test:value";
        final Assertion apAssertion = Assertion.createAnnotationPropertyAssertion(PROPERTY, false);
        final Value res = sut.toRdf4jValue(apAssertion, value(value));
        assertTrue(res instanceof Literal);
        final Literal literal = (Literal) res;
        assertFalse(literal.getLanguage().isPresent());
    }
}
