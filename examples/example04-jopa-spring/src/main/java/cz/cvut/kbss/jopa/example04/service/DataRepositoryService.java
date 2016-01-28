package cz.cvut.kbss.jopa.example04.service;

import cz.cvut.kbss.jopa.example04.persistence.dao.DataDao;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Objects;

@Service
public class DataRepositoryService {

    @Autowired
    private DataDao dataDao;

    public String getRepositoryData(String format) {
        Objects.requireNonNull(format);
        final DataFormat df = DataFormat.fromString(format);
        return dataDao.getRepositoryData(df);
    }
}
