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
package cz.cvut.kbss.ontodriver.owlapi.query;

import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.owl2query.model.QueryResult;
import org.semanticweb.owlapi.model.OWLObject;

import java.util.Observer;

public abstract class AbstractResultSet implements ResultSet {

    private boolean open;
    private final Statement statement;

    protected AbstractResultSet(Statement statement) {
        this.statement = statement;
        this.open = true;
    }

    void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("The result set is closed.");
        }
    }

    @Override
    public Statement getStatement() {
        return statement;
    }

    @Override
    public void close() {
        this.open = false;
    }

    @Override
    public boolean isOpen() {
        return open;
    }

    @Override
    public void registerObserver(Observer observer) {
        throw new UnsupportedOperationException("Not supported by the current version.");
    }

    public static ResultSet createResultSet(QueryResult<OWLObject> result, Statement statement, String query) {
        if (isAskQuery(query)) {
            return new AskResultSet(result, statement);
        } else {
            return new SelectResultSet(result, statement);
        }
    }

    private static boolean isAskQuery(String statement) {
        return statement.toLowerCase().startsWith("ask");
    }
}
