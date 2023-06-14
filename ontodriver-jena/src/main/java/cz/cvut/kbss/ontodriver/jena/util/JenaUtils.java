/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.jena.util;

import cz.cvut.kbss.jopa.datatype.xsd.XsdDatatypeMapper;
import cz.cvut.kbss.jopa.datatype.xsd.XsdTemporalMapper;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.util.IdentifierUtils;
import org.apache.jena.datatypes.BaseDatatype;
import org.apache.jena.datatypes.RDFDatatype;
import org.apache.jena.datatypes.TypeMapper;
import org.apache.jena.datatypes.xsd.impl.RDFLangString;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.ResourceFactory;

import java.time.temporal.TemporalAccessor;
import java.time.temporal.TemporalAmount;
import java.util.Date;
import java.util.Objects;

/**
 * Utility methods for working with Jena API.
 */
public class JenaUtils {

    private JenaUtils() {
        throw new AssertionError();
    }

    /**
     * Transforms the specified value to an {@link RDFNode}, be it a resource or a literal.
     *
     * @param assertion Assertion representing the asserted property
     * @param value     Value to transform
     * @return Jena RDFNode
     */
    public static <T> RDFNode valueToRdfNode(Assertion assertion, Value<T> value) {
        final T val = value.getValue();
        Objects.requireNonNull(val);
        if (IdentifierUtils.isResourceIdentifierType(val.getClass())) {
            return ResourceFactory.createResource(value.stringValue());
        } else if (val instanceof LangString) {
            final LangString langString = (LangString) val;
            return langString.getLanguage().map(lang -> ResourceFactory.createLangLiteral(langString.getValue(), lang))
                             .orElseGet(() -> ResourceFactory.createTypedLiteral(langString.getValue()));
        } else if (val instanceof String) {
            return assertion.hasLanguage() ? ResourceFactory.createLangLiteral((String) val,
                                                                               assertion.getLanguage()) :
                   ResourceFactory.createTypedLiteral(val);
        } else if (val instanceof cz.cvut.kbss.ontodriver.model.Literal) {
            final cz.cvut.kbss.ontodriver.model.Literal ontoLiteral = (cz.cvut.kbss.ontodriver.model.Literal) val;
            return createLiteral(ontoLiteral);
        } else if (val instanceof Date) {
            final cz.cvut.kbss.ontodriver.model.Literal ontoLiteral = XsdTemporalMapper.map(((Date) val).toInstant());
            return createLiteral(ontoLiteral);
        } else if (val instanceof TemporalAccessor) {
            final cz.cvut.kbss.ontodriver.model.Literal ontoLiteral = XsdTemporalMapper.map(((TemporalAccessor) val));
            return createLiteral(ontoLiteral);
        } else if (val instanceof TemporalAmount) {
            final cz.cvut.kbss.ontodriver.model.Literal ontoLiteral = XsdTemporalMapper.map(((TemporalAmount) val));
            return createLiteral(ontoLiteral);
        } else {
            return ResourceFactory.createTypedLiteral(val);
        }
    }

    private static Literal createLiteral(cz.cvut.kbss.ontodriver.model.Literal ontoLiteral) {
        final TypeMapper typeMapper = TypeMapper.getInstance();
        RDFDatatype datatype = typeMapper.getTypeByName(ontoLiteral.getDatatype());
        if (datatype == null) {
            // If the datatype does not exist, register it as a base datatype without any special behavior
            datatype = new BaseDatatype(ontoLiteral.getDatatype());
            typeMapper.registerDatatype(datatype);
        }
        return ResourceFactory.createTypedLiteral(ontoLiteral.getLexicalForm(), datatype);
    }

    public static Object literalToValue(Literal literal) {
        if (RDFLangString.rdfLangString.equals(literal.getDatatype())) {
            return new LangString(literal.getString(), literal.getLanguage());
        }
        final cz.cvut.kbss.ontodriver.model.Literal lit = cz.cvut.kbss.ontodriver.model.Literal.from(
                literal.getLexicalForm(), literal.getDatatypeURI());
        return XsdDatatypeMapper.getInstance().map(lit).orElse(lit);
    }
}
