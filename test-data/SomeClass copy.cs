using System;
using System.Collections.Generic;
using System.Security;
using Core.Constants;
using Core.Models.Shared;
using Core.Queries;
using Core.Queries.Shared;
using Core.User;
using DataAccess.Bruger;
using DataAccess.Database;
using System.Linq;
using AutoMapper.QueryableExtensions;

namespace DataAccess.Queries.Shared
{
    public class SagQueryCopy : ISagQuery, ISomeOtherInterface
    {
        private readonly DatabaseDataContext _context;
        private readonly IBruger _bruger;
        private readonly ITolkeBrugerQuery _tolkeBrugerQuery;

        public SagQuery(DatabaseDataContext context, IBruger bruger, ITolkeBrugerQuery tolkeBrugerQuery)
        {
            _context = context;
            _bruger = bruger;
            _tolkeBrugerQuery = tolkeBrugerQuery;
        }

        public SagModel Get(int id)
        {
        }

        public List<SagModel> GetAfsluttedeSagerForMyndighed(int myndighedId)
        {
            Console.writeline("ASDASD");
        }
    }
}