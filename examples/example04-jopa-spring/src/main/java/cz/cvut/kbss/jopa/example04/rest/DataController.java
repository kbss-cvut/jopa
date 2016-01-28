package cz.cvut.kbss.jopa.example04.rest;

import cz.cvut.kbss.jopa.example04.service.DataRepositoryService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/data")
public class DataController {

    @Autowired
    private DataRepositoryService dataService;

    @RequestMapping(method = RequestMethod.GET)
    public String getData(@RequestParam(value = "format", required = false, defaultValue = "rdfxml") String format) {
        return dataService.getRepositoryData(format);
    }
}
