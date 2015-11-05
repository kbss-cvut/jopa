/**
 * Copyright (C) 2011 Czech Technical University in Prague
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

package cz.cvut.kbss.jopa.owlapi;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import cz.cvut.kbss.jopa.exceptions.NoResultException;
import cz.cvut.kbss.jopa.exceptions.NoUniqueResultException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.query.Parameter;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;
import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

public class TypedQueryImpl<ResultElement> implements TypedQuery<ResultElement> {

    private final String query;
    private final Set<URI> contexts;
    private final Class<ResultElement> classT;
    private final ConnectionWrapper connection;
    private final MetamodelProvider metamodelProvider;

    private UnitOfWork uow;

    private boolean useBackupOntology;
    private int maxResults;

    public TypedQueryImpl(final String query, final Class<ResultElement> classT, final ConnectionWrapper connection,
                          MetamodelProvider metamodelProvider) {
        this.query = Objects.requireNonNull(query, ErrorUtils.constructNPXMessage("query"));
        this.classT = Objects.requireNonNull(classT, ErrorUtils.constructNPXMessage("classT"));
        this.connection = Objects.requireNonNull(connection,
                ErrorUtils.constructNPXMessage("connection"));
        this.metamodelProvider = Objects.requireNonNull(metamodelProvider,
                ErrorUtils.constructNPXMessage("metamodelProvider"));
        this.contexts = new HashSet<>();
        this.maxResults = Integer.MAX_VALUE;
    }

    public void setUnitOfWork(UnitOfWork uow) {
        this.uow = uow;
    }

    @Override
    public List<ResultElement> getResultList() {
        if (maxResults == 0) {
            return Collections.emptyList();
        }

        List<ResultElement> list;
        try {
            list = getResultListImpl(maxResults);
        } catch (OntoDriverException e) {
            throw new OWLPersistenceException("Exception caught when evaluating query " + query, e);
        }

        return list;
    }

    @Override
    public ResultElement getSingleResult() {
        try {
            // call it with maxResults = 2 just to see whether there are
            // multiple results
            final List<ResultElement> res = getResultListImpl(2);
            if (res.isEmpty()) {
                throw new NoResultException("No result found for query " + query);
            }
            if (res.size() > 1) {
                throw new NoUniqueResultException("Multiple results found for query " + query);
            }
            return res.get(0);
        } catch (OntoDriverException e) {
            throw new OWLPersistenceException("Exception caught when evaluating query " + query, e);
        }
    }

    @Override
    public TypedQuery<ResultElement> setMaxResults(int maxResults) {
        if (maxResults < 0) {
            throw new IllegalArgumentException(
                    "Cannot set maximum number of results to less than 0.");
        }
        this.maxResults = maxResults;
        return this;
    }

    @Override
    public int getMaxResults() {
        return maxResults;
    }

    @Override
    public Parameter<?> getParameter(int position) {
        return null;
    }

    @Override
    public Parameter<?> getParameter(String name) {
        return null;
    }

    @Override
    public Set<Parameter<?>> getParameters() {
        return null;
    }

    @Override
    public Object getParameterValue(int position) {
        return null;
    }

    @Override
    public Object getParameterValue(String name) {
        return null;
    }

    @Override
    public <T> T getParameterValue(Parameter<T> parameter) {
        return null;
    }

    @Override
    public Query<ResultElement> setParameter(int position, Object value) {
        return null;
    }

    @Override
    public Query<ResultElement> setParameter(int position, String value, String language) {
        return null;
    }

    @Override
    public Query<ResultElement> setParameter(String name, Object value) {
        return null;
    }

    @Override
    public Query<ResultElement> setParameter(String name, String value, String language) {
        return null;
    }

    @Override
    public <T> Query<ResultElement> setParameter(Parameter<T> parameter, T value) {
        return null;
    }

    @Override
    public Query<ResultElement> setParameter(Parameter<String> parameter, String value, String language) {
        return null;
    }

    private List<ResultElement> getResultListImpl(int maxResults) throws OntoDriverException {
        assert maxResults > 0;
        final Statement stmt = connection.createStatement();
        if (useBackupOntology) {
            stmt.setUseBackupOntology();
        } else {
            stmt.setUseTransactionalOntology();
        }
        URI[] arr = new URI[contexts.size()];
        arr = contexts.toArray(arr);
        final ResultSet rs = stmt.executeQuery(query, arr);
        try {
            final List<ResultElement> res = new ArrayList<>();
            // TODO register this as observer on the result set so that additional results can be loaded asynchronously
            int cnt = 0;
            final URI ctx = arr.length > 0 ? arr[0] : null;
            final boolean isTypeManaged = metamodelProvider.isTypeManaged(classT);
            while (rs.hasNext() && cnt < maxResults) {
                rs.next();
                if (isTypeManaged) {
                    res.add(loadEntityInstance(rs, ctx));
                } else {
                    res.add(loadResultValue(rs));
                }
                cnt++;
            }
            return res;
        } finally {
            rs.close();
        }
    }

    private ResultElement loadEntityInstance(ResultSet resultSet, URI context) throws OntoDriverException {
        if (uow == null) {
            throw new IllegalStateException("Cannot load entity instance without Unit of Work.");
        }
        final URI uri = URI.create(resultSet.getString(0));
        // TODO Setting the context like this won't work for queries over multiple contexts
        final EntityDescriptor descriptor = new EntityDescriptor(context);

        final ResultElement entity = uow.readObject(classT, uri, descriptor);
        if (entity == null) {
            throw new OWLPersistenceException(
                    "Fatal error, unable to load entity for primary key already found by query "
                            + query);
        }
        return entity;
    }

    private ResultElement loadResultValue(ResultSet resultSet) throws OntoDriverException {
        try {
            return resultSet.getObject(0, classT);
        } catch (OntoDriverException e) {
            throw new OWLPersistenceException("Unable to map the query result to class " + classT, e);
        }
    }

    @Override
    public Query<ResultElement> addContext(URI context) {
        Objects.requireNonNull(context, ErrorUtils.constructNPXMessage("context"));
        contexts.add(context);
        return this;
    }

    @Override
    public Query<ResultElement> addContexts(Collection<URI> contexts) {
        Objects.requireNonNull(contexts, ErrorUtils.constructNPXMessage("contexts"));
        this.contexts.addAll(contexts);
        return this;
    }

    @Override
    public Query<ResultElement> clearContexts() {
        contexts.clear();
        return this;
    }

    /**
     * Sets ontology used for processing of this query. </p>
     *
     * @param useBackupOntology If true, the backup (central) ontology is used, otherwise the
     *                          transactional ontology is used (default)
     */
    public void setUseBackupOntology(boolean useBackupOntology) {
        this.useBackupOntology = useBackupOntology;
    }
}
