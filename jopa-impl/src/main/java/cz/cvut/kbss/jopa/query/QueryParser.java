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
package cz.cvut.kbss.jopa.query;

/**
 * Used to parse queries into builders which enable the query to be further manipulated, e.g. set parameters.
 *
 * @author kidney
 */
public interface QueryParser {

    /**
     * Parses the specified query string and returns a query builder instance containing the parsed query.
     *
     * @param query The query to parse
     * @return Query builder with the parsed query
     */
    QueryHolder parseQuery(String query);
}
