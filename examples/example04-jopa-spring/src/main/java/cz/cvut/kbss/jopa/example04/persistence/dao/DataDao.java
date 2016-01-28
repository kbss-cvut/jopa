package cz.cvut.kbss.jopa.example04.persistence.dao;

import cz.cvut.kbss.jopa.example04.service.DataFormat;
import org.openrdf.repository.Repository;
import org.openrdf.repository.RepositoryConnection;
import org.openrdf.repository.RepositoryException;
import org.openrdf.rio.RDFFormat;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;
import org.openrdf.rio.rdfjson.RDFJSONWriter;
import org.openrdf.rio.rdfxml.util.RDFXMLPrettyWriter;
import org.openrdf.rio.turtle.TurtleWriter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;

@Component
public class DataDao {

    private static final Logger LOG = LoggerFactory.getLogger(DataDao.class);

    @Autowired
    private Repository repository;

    public String getRepositoryData(DataFormat format) {
        try {
            final RepositoryConnection connection = repository.getConnection();
            final ByteArrayOutputStream bos = new ByteArrayOutputStream();
            final RDFHandler rdfHandler = getHandler(format, bos);
            connection.export(rdfHandler);
            connection.close();
            return new String(bos.toByteArray());
        } catch (RepositoryException | RDFHandlerException e) {
            LOG.error("Unable to read data from repository.", e);
            return "";
        }
    }

    private RDFHandler getHandler(DataFormat format, OutputStream os) {
        switch (format) {
            case JSON:
                return new RDFJSONWriter(os, RDFFormat.RDFJSON);
            case RDFXML:
                return new RDFXMLPrettyWriter(os);
            case TURTLE:
                return new TurtleWriter(os);
            default:
                throw new IllegalArgumentException("Unsupported data format: " + format);
        }
    }
}
