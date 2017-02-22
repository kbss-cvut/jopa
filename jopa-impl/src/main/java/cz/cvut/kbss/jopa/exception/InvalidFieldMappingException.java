/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.exception;

/**
 * Signals that an entity field mapping is not valid.
 * <p>
 * It can mean for example that its type does not correspond to the mapping (singular field for plural mapping,
 * non-Set for a {@link cz.cvut.kbss.jopa.model.annotations.Types} field) etc.
 */
public class InvalidFieldMappingException extends MetamodelInitializationException {

    public InvalidFieldMappingException(String message) {
        super(message);
    }
}
