using CardDemo.POC.Web.Data.Entities;
using Microsoft.EntityFrameworkCore;

namespace CardDemo.POC.Web.Data;

/// <summary>
/// Database context for CardDemo POC application
/// </summary>
public class CardDemoDbContext : DbContext
{
    public CardDemoDbContext(DbContextOptions<CardDemoDbContext> options)
        : base(options)
    {
    }

    public DbSet<Customer> Customers { get; set; } = null!;
    public DbSet<Account> Accounts { get; set; } = null!;
    public DbSet<Card> Cards { get; set; } = null!;
    public DbSet<Transaction> Transactions { get; set; } = null!;

    protected override void OnModelCreating(ModelBuilder modelBuilder)
    {
        base.OnModelCreating(modelBuilder);

        // Configure Customer
        modelBuilder.Entity<Customer>(entity =>
        {
            entity.HasKey(e => e.CustomerId);
            entity.Property(e => e.CustomerId).HasMaxLength(9).IsRequired();
            entity.Property(e => e.FirstName).HasMaxLength(25).IsRequired();
            entity.Property(e => e.MiddleName).HasMaxLength(25);
            entity.Property(e => e.LastName).HasMaxLength(25).IsRequired();
            entity.Property(e => e.PhoneNumber).HasMaxLength(20);
            entity.Property(e => e.FicoCreditScore).IsRequired();
        });

        // Configure Account
        modelBuilder.Entity<Account>(entity =>
        {
            entity.HasKey(e => e.AccountId);
            entity.Property(e => e.AccountId).HasMaxLength(11).IsRequired();
            entity.Property(e => e.CustomerId).HasMaxLength(9).IsRequired();
            entity.Property(e => e.CreditLimit).HasPrecision(11, 2);
            entity.Property(e => e.CurrentBalance).HasPrecision(11, 2);
            entity.Property(e => e.Status).HasMaxLength(1).IsRequired();
            entity.HasIndex(e => e.CustomerId);

            // Relationship to Customer
            entity.HasOne(e => e.Customer)
                  .WithMany(c => c.Accounts)
                  .HasForeignKey(e => e.CustomerId);
        });

        // Configure Card
        modelBuilder.Entity<Card>(entity =>
        {
            entity.HasKey(e => e.CardNumber);
            entity.Property(e => e.CardNumber).HasMaxLength(16).IsRequired();
            entity.Property(e => e.AccountId).HasMaxLength(11).IsRequired();
            entity.Property(e => e.CardType).HasMaxLength(10).IsRequired();
            entity.Property(e => e.Status).HasMaxLength(1).IsRequired();
            entity.HasIndex(e => e.AccountId);

            // Relationship to Account
            entity.HasOne(e => e.Account)
                  .WithMany(a => a.Cards)
                  .HasForeignKey(e => e.AccountId);
        });

        // Configure Transaction
        modelBuilder.Entity<Transaction>(entity =>
        {
            entity.HasKey(e => e.TransactionId);
            entity.Property(e => e.TransactionId).HasMaxLength(16).IsRequired();
            entity.Property(e => e.AccountId).HasMaxLength(11).IsRequired();
            entity.Property(e => e.CardNumber).HasMaxLength(16).IsRequired();
            entity.Property(e => e.TransactionTypeCode).HasMaxLength(2).IsRequired();
            entity.Property(e => e.TransactionCategoryCode).HasMaxLength(4).IsRequired();
            entity.Property(e => e.Amount).HasPrecision(11, 2);
            entity.Property(e => e.Description).HasMaxLength(100);
            entity.HasIndex(e => e.AccountId);
            entity.HasIndex(e => e.TransactionDate);

            // Relationship to Account
            entity.HasOne(e => e.Account)
                  .WithMany(a => a.Transactions)
                  .HasForeignKey(e => e.AccountId);
        });
    }
}
