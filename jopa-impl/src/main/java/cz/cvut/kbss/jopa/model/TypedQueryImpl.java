/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.exceptions.NoResultException;
import cz.cvut.kbss.jopa.exceptions.NoUniqueResultException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.query.Parameter;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.query.QueryHolder;
import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;
import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

import java.net.URI;
import java.util.*;

public class TypedQueryImpl<X> extends AbstractQuery implements TypedQuery<X> {

    private final Class<X> resultType;
    private final MetamodelProvider metamodelProvider;

    private UnitOfWork uow;

    private Descriptor descriptor;

    public TypedQueryImpl(final QueryHolder query, final Class<X> resultType,
                          final ConnectionWrapper connection, MetamodelProvider metamodelProvider) {
        super(query, connection);
        this.resultType = Objects.requireNonNull(resultType, ErrorUtils.getNPXMessageSupplier("resultType"));
        this.metamodelProvider = Objects
                .requireNonNull(metamodelProvider, ErrorUtils.getNPXMessageSupplier("metamodelProvider"));
    }

    public void setUnitOfWork(UnitOfWork uow) {
        this.uow = uow;
    }

    @Override
    public void executeUpdate() {
        executeUpdateImpl();
    }

    @Override
    public List<X> getResultList() {
        if (maxResults == 0) {
            return Collections.emptyList();
        }
        List<X> list;
        try {
            list = getResultListImpl(maxResults);
        } catch (OntoDriverException e) {
            markTransactionForRollback();
            throw queryEvaluationException(e);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }

        return list;
    }

    @Override
    public X getSingleResult() {
        try {
            // call it with maxResults = 2 just to see whether there are
            // multiple results
            final List<X> res = getResultListImpl(2);
            if (res.isEmpty()) {
                throw new NoResultException("No result found for query " + query);
            }
            if (res.size() > 1) {
                throw new NoUniqueResultException("Multiple results found for query " + query);
            }
            return res.get(0);
        } catch (OntoDriverException e) {
            markTransactionForRollback();
            throw queryEvaluationException(e);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
    }

    @Override
    public TypedQuery<X> setMaxResults(int maxResults) {
        if (maxResults < 0) {
            markTransactionForRollback();
            throw new IllegalArgumentException("Cannot set maximum number of results to less than 0.");
        }
        this.maxResults = maxResults;
        return this;
    }

    @Override
    public TypedQuery<X> setParameter(int position, Object value) {
        try {
            query.setParameter(query.getParameter(position), value);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
        return this;
    }

    @Override
    public TypedQuery<X> setParameter(int position, String value, String language) {
        try {
            query.setParameter(query.getParameter(position), value, language);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
        return this;
    }

    @Override
    public TypedQuery<X> setParameter(String name, Object value) {
        try {
            query.setParameter(query.getParameter(name), value);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
        return this;
    }

    @Override
    public TypedQuery<X> setParameter(String name, String value, String language) {
        try {
            query.setParameter(query.getParameter(name), value, language);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
        return this;
    }

    @Override
    public <T> TypedQuery<X> setParameter(Parameter<T> parameter, T value) {
        try {
            query.setParameter(parameter, value);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
        return this;
    }

    @Override
    public TypedQuery<X> setParameter(Parameter<String> parameter, String value, String language) {
        try {
            query.setParameter(parameter, value, language);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
        return this;
    }

    @Override
    public TypedQuery<X> setDescriptor(Descriptor descriptor) {
        this.descriptor = descriptor;
        return this;
    }

    private List<X> getResultListImpl(int maxResults) throws OntoDriverException {
        assert maxResults > 0;
        final Statement stmt = connection.createStatement();
        try {
            setTargetOntology(stmt);
            logQuery();
            final ResultSet rs = stmt.executeQuery(query.assembleQuery());
            final List<X> res = new ArrayList<>();
            // TODO register this as observer on the result set so that additional results can be loaded asynchronously
            int cnt = 0;
            final boolean isTypeManaged = metamodelProvider.isTypeManaged(resultType);
            final Descriptor instDescriptor = descriptor != null ? descriptor : new EntityDescriptor();
            while (rs.hasNext() && cnt < maxResults) {
                rs.next();
                if (isTypeManaged) {
                    loadEntityInstance(rs, instDescriptor).ifPresent(res::add);
                } else {
                    res.add(loadResultValue(rs));
                }
                cnt++;
            }
            return res;
        } finally {
            try {
                stmt.close();
            } catch (Exception e) {
                LOG.error("Unable to close statement after query evaluation.", e);
            }
        }
    }

    private Optional<X> loadEntityInstance(ResultSet resultSet, Descriptor instanceDescriptor)
            throws OntoDriverException {
        if (uow == null) {
            throw new IllegalStateException("Cannot load entity instance without Unit of Work.");
        }
        final URI uri = URI.create(resultSet.getString(0));
        return Optional.ofNullable(uow.readObject(resultType, uri, instanceDescriptor));
    }

    private X loadResultValue(ResultSet resultSet) throws OntoDriverException {
        try {
            return resultSet.getObject(0, resultType);
        } catch (OntoDriverException e) {
            throw new OWLPersistenceException("Unable to map the query result to class " + resultType, e);
        }
    }
}
