/**
 * Copyright (C) 2019 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.model.annotations;

import java.lang.annotation.*;

@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface OWLDataProperty {

    /**
     * IRI of the data property
     *
     * @return IRI of the data property
     */
    String iri();

    /**
     * (Optional) Whether the association should be lazily loaded or must be eagerly fetched.
     *
     * @return Fetch type of this property
     */
    FetchType fetch() default FetchType.EAGER;

    /**
     * Marks an attribute whose value is a lexical form of a literal value.
     * <p>
     * This parameter should be used on {@code String} attributes, as literal lexical form is always a string. Lexical
     * form of a literal of any datatype can be loaded. However, saving the lexical form is forbidden to prevent
     * unintentional change of the data type.
     */
    boolean lexicalForm() default false;
}
