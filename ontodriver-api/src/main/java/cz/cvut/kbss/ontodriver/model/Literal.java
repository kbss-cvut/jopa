/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.ontodriver.model;

import java.io.Serializable;
import java.util.Objects;

/**
 * Represents an RDF literal consisting of a lexical form and datatype IRI.
 * <p>
 * To represent language-tagged string values, use {@link LangString}.
 */
public class Literal implements Serializable {

    private final String lexicalForm;

    private final String datatype;

    public Literal(String lexicalForm, String datatype) {
        this.lexicalForm = Objects.requireNonNull(lexicalForm);
        this.datatype = Objects.requireNonNull(datatype);
    }

    /**
     * Gets the lexical form of this literal.
     *
     * @return String containing the lexical form
     */
    public String getLexicalForm() {
        return lexicalForm;
    }

    /**
     * Gets the datatype identifier.
     *
     * @return String containing the datatype IRI
     */
    public String getDatatype() {
        return datatype;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof Literal literal)) {
            return false;
        }
        return lexicalForm.equals(literal.lexicalForm) && datatype.equals(literal.datatype);
    }

    @Override
    public int hashCode() {
        return Objects.hash(lexicalForm, datatype);
    }

    @Override
    public String toString() {
        return "\"" + lexicalForm + "\"^^<" + datatype + ">";
    }

    /**
     * Creates a literal instance from the specified lexical form and datatype.
     * <p>
     * Convenience factory method alternative to constructor.
     *
     * @param lexicalForm Lexical form of the literal
     * @param datatype    Datatype of the literal
     * @return Literal instance
     */
    public static Literal from(String lexicalForm, String datatype) {
        return new Literal(lexicalForm, datatype);
    }
}
