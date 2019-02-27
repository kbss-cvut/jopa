/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.exception;

/**
 * Thrown when an attempt is made to extract value of a variable which is not bound by the {@link cz.cvut.kbss.ontodriver.ResultSet}.
 * <p>
 * This can happen for example when an OPTIONAL operator is used, so the variable is present in the result set, but
 * may not be bound by the current row.
 */
public class VariableNotBoundException extends OntoDriverRuntimeException {

    public VariableNotBoundException(String message) {
        super(message);
    }
}
