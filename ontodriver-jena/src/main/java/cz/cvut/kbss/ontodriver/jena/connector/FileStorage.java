package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.exception.OntoDriverInitializationException;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.DatasetFactory;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.shared.NotFoundException;
import org.apache.jena.util.FileManager;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

class FileStorage extends Storage {

    // TODO Write changes to the file (on close, on commit?)
    private final String location;

    FileStorage(Configuration configuration) {
        super(configuration);
        this.location = configuration.getStorageProperties().getPhysicalURI().toString();
    }

    @Override
    public void initialize() {
        try {
            try {
                initDataset();
            } catch (NotFoundException e) {
                tryCreatingFile();
            }
        } catch (RuntimeException e) {
            throw new OntoDriverInitializationException("Unable to initialize file storage at " + location, e);
        }
    }

    private void initDataset() {
        final Model model = FileManager.get().loadModel(location);
        this.dataset = DatasetFactory.create(model);
    }

    private void tryCreatingFile() {
        final File file = new File(location);
        try {
            final boolean result = file.createNewFile();
            assert result;
        } catch (IOException e) {
            LOG.error("Unable to create storage file {}.", location);
            throw new OntoDriverInitializationException(e);
        }
        initDataset();
    }

    @Override
    public Dataset getDataset() {
        return dataset;
    }

    @Override
    void writeChanges() throws OntoDriverException {
        try (final BufferedOutputStream out = new BufferedOutputStream(new FileOutputStream(location))) {
            // TODO What language to use, i.e. how to determine it from input
            RDFDataMgr.write(out, dataset, Lang.TRIG);
        } catch (IOException e) {
            throw new OntoDriverException("Unable to write out dataset changes.", e);
        }
    }

    @Override
    public void close() {

    }
}
