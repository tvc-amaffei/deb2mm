def mm(data_table, mm_table):

    import numpy as np
    import re

    # because of row precedence and accumulation of matches across columns we must iterate


    # determine which columns in mm_table are output columns
    output_columns = set(mm_table.columns) - set(data_table.columns)

    result_rows = []

    for data_row in data_table.itertuples(): # for each data_row
        i_row = data_row._asdict()
        result_row = {}
        for mm in mm_table.itertuples(): # look at each mm rule
            mm_row = mm._asdict()
            groups = {}
            row_matched = True
            for col in data_table.columns: # regex match and accumulate groups
                regex = mm_row[col]
                if not regex: # no regex to match
                    continue
                regex = '^{}$'.format(regex) # match whole string
                m = re.match(regex, i_row[col])
                if m is not None: # matched
                    # accumulate groups
                    groups.update(m.groupdict())
                else: # did not match
                    row_matched = False
            if not row_matched: # whole row must match
                continue
            for col in output_columns:
                if not mm_row[col]:
                    # there's no default value, don't fill it in
                    continue
                if mm_row[col].startswith('<-'): # arbitrary Python expression
                    expr = mm_row[col][2:]
                    result_row[col] = eval(expr, None, groups)
                else:
                    try:
                        # interpolate the groups into the value of the cell
                        result_row[col] = mm_row[col].format(**groups)
                    except KeyError:
                        # a group in the format string is not present
                        # in the accumulated groups, this is an error
                        # in the m/m table
                        raise
        result_rows.append(result_row)

    output_table = pd.DataFrame(result_rows).fillna('')
    output_table
