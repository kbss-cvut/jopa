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
package cz.cvut.kbss.ontodriver.owlapi.util;

import cz.cvut.kbss.jopa.datatype.xsd.XsdDatatypeMapper;
import cz.cvut.kbss.jopa.owlapi.DatatypeTransformer;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.Literal;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.owlapi.config.Constants;
import cz.cvut.kbss.ontodriver.util.IdentifierUtils;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.vocab.OWL2Datatype;

import java.net.URI;
import java.net.URL;

/**
 * Utility methods for the OWLAPI driver.
 */
public class OwlapiUtils {

    private OwlapiUtils() {
        throw new AssertionError("Can't create instance.");
    }

    /**
     * Creates OWLLiteral from the specified Java instance.
     *
     * @param value The value to transform
     * @param lang  Ontology language
     * @return OWLLiteral representing the value
     * @throws IllegalArgumentException If {@code value} is of unsupported type
     */
    public static OWLLiteral createOWLLiteralFromValue(Object value, String lang) {
        return DatatypeTransformer.transform(value, lang);
    }

    /**
     * Transforms OWLLiteral to a plain Java object (boxed primitive or date/time).
     *
     * @param owlLiteral The literal to transform
     * @return Transformed value
     * @throws IllegalArgumentException If the literal is of unsupported type
     */
    public static Object owlLiteralToValue(final OWLLiteral owlLiteral) {
        final OWLDatatype datatype = owlLiteral.getDatatype();
        if (datatype.isBuiltIn() && datatype.getBuiltInDatatype() == OWL2Datatype.RDF_LANG_STRING) {
            return new LangString(owlLiteral.getLiteral(), owlLiteral.getLang());
        }
        final Literal literal = Literal.from(owlLiteral.getLiteral(), owlLiteral.getDatatype().toStringID());
        return XsdDatatypeMapper.getInstance().map(literal).orElse(literal);
    }

    /**
     * Checks whether the specified literal matches to the language of the specified assertion.
     * <p>
     * If the literal is not a string, it automatically matches. If it is a string, it matches if assertion language is
     * not specified, it is without language tag or if the language tag matches the specified assertion language.
     *
     * @param literal   Literal to check
     * @param assertion Assertion with language specification (possibly empty)
     * @return {@code true} if the literal matches the assertion language, {@code false} otherwise
     */
    public static boolean doesLanguageMatch(OWLLiteral literal, Assertion assertion) {
        assert literal != null;
        assert assertion != null;

        return !assertion.hasLanguage() || literal.getLang().isEmpty() ||
                literal.getLang().equals(assertion.getLanguage());
    }

    /**
     * Retrieves language from the specified assertion.
     * <p>
     * If the assertion does not specify language, {@link Constants#DEFAULT_LANGUAGE} is returned.
     *
     * @param assertion Assertion to get language for
     * @return Assertion language or default language
     */
    public static String getAssertionLanguage(Assertion assertion) {
        return assertion.hasLanguage() ? assertion.getLanguage() : Constants.DEFAULT_LANGUAGE;
    }

    /**
     * Gets OWLNamedIndividual for the specified named resource.
     *
     * @param subject     Named resource to transform to individual
     * @param dataFactory OWL data factory
     * @return OWLNamedIndividual
     */
    public static OWLNamedIndividual getIndividual(NamedResource subject, OWLDataFactory dataFactory) {
        return dataFactory.getOWLNamedIndividual(IRI.create(subject.getIdentifier()));
    }

    /**
     * Checks whether the specified value is a valid IRI.
     * <p>
     * Only absolute IRIs are accepted.
     *
     * @param value The value to check
     * @return {@code true} for instances of {@link NamedResource}, {@link URI}, {@link URL} or {@link IRI} and for
     * Strings parseable by {@link URI#create(String)}.
     */
    public static boolean isIndividualIri(Object value) {
        return IdentifierUtils.isResourceIdentifierType(value.getClass()) || value instanceof IRI;
    }
}
