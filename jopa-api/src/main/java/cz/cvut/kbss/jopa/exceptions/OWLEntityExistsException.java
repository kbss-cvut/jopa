/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.exceptions;

/**
 * Thrown when an attempt to put an entity with duplicate identifier into the persistence context is made.
 */
public class OWLEntityExistsException extends OWLPersistenceException {

    private static final long serialVersionUID = 453666323423782580L;

    public OWLEntityExistsException(String message) {
        super(message);
    }

    public static OWLEntityExistsException individualAlreadyManaged(Object identifier) {
        return new OWLEntityExistsException(
                "Individual <" + identifier + "> is already managed in the persistence context.");
    }
}
