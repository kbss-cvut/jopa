/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model;

/**
 * A class that implements this interface can be used to convert entity attribute state into axiom/triple value
 * representation and back again.
 * <p>
 * Note that the X and Y types may be the same Java type.
 *
 * @param <X> the type of the entity attribute
 * @param <Y> the type of the axiom/triple value
 */
public interface AttributeConverter<X, Y> {

    /**
     * Converts the value stored in the entity attribute into the data representation to be stored in the repository and
     * supported by the OntoDriver API.
     *
     * @param value Value to convert
     * @return Converted data
     */
    Y convertToAxiomValue(X value);

    /**
     * Converts the data stored in the repository into the value to be stored in the entity attribute.
     * <p>
     * Note that it is the responsibility of the converter writer to specify the correct {@code value} type for the
     * corresponding value for use by the OntoDriver: i.e., persistence providers are not expected to do such type
     * conversion.
     *
     * @param value Value to convert
     * @return Converted data
     */
    X convertToAttribute(Y value);
}
